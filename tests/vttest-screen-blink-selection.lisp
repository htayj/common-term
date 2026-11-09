;;;; Pure replay of native blink phase selections and their link to a capture's
;;;; original READY status. Validators take untrusted evidence and return errors
;;;; as data. They check each status's finite shape before comparing any value, so
;;;; atoms, dotted or circular lists and circular IDs are refused, never looped on.
(load (merge-pathnames "vttest-screen-blink-timing.lisp" *load-truename*))
(defpackage #:vttest-screen-blink-selection
  (:use #:cl #:vttest-screen-blink-timing)
  (:export #:*source-keys* #:*selection-record-keys* #:proper-list-p #:plist-p
           #:record-keys #:well-formed-p #:same-receipt-p #:precedes-or-equal-p
           #:ready-linked-p #:expected-blink-phase #:validate-blink-selection
           #:blink-selection-record #:validate-selection-record #:chain-report))
(in-package #:vttest-screen-blink-selection)

(defparameter *source-keys* '(:id :bytes-received :blink-epoch :blink-epoch-at)
  "Keys naming one source, byte position and blink epoch.")
(defparameter *generation-keys*
  '(:blink-phase :blink-toggled-at :blink-previous-at :blink-toggle-frame)
  "Receipt keys fixed within one toggle generation.")
(defparameter *selection-record-keys* '(:kind :version :pause :phase :target :polls))

(defun proper-list-p (x)
  "True for a finite NIL-terminated list."
  (and (listp x) (handler-case (list-length x) (error () nil)) t))
(defun plist-p (x) (and (proper-list-p x) (evenp (length x))))
(defun record-keys (plist) (loop for key in plist by #'cddr collect key))
(defun same-keys-p (a b keys)
  (every (lambda (key) (equal (getf a key) (getf b key))) keys))

(defun finite-tree-p (x &optional (limit 4096))
  "True when X holds no cycle or shared cons and at most LIMIT conses."
  (let ((seen (make-hash-table :test 'eq)))
    (labels ((walk (y)
               (loop (cond ((atom y) (return t))
                           ((or (gethash y seen) (>= (hash-table-count seen) limit))
                            (return nil))
                           (t (setf (gethash y seen) t)
                              (unless (walk (car y)) (return nil))
                              (setf y (cdr y)))))))
      (walk x))))

(defun well-formed-p (status)
  "True when STATUS is a finite acyclic plist with well-formed receipts and a scalar
:ID (string, integer or symbol), so every later EQUAL on it terminates."
  (and (finite-tree-p status) (null (blink-receipt-errors status))
       (typep (getf status :id) '(or string integer symbol))))

(defun same-receipt-p (a b)
  "True when well-formed A and B carry the same :ID and receipt."
  (and (well-formed-p a) (well-formed-p b) (same-keys-p a b (cons :id *receipt-keys*))))

(defun precedes-or-equal-p (a b)
  "True when status A was observed no later than B from one source and epoch: A's
frames and toggles are at most B's, one generation keeps one receipt, and a later
generation has a strictly later toggle frame."
  (and (well-formed-p a) (well-formed-p b) (same-keys-p a b *source-keys*)
       (<= (getf a :frames) (getf b :frames))
       (let ((toggles-a (getf a :blink-toggles)) (toggles-b (getf b :blink-toggles)))
         (cond ((= toggles-a toggles-b) (same-keys-p a b *generation-keys*))
               ((< toggles-a toggles-b)
                (or (zerop toggles-a)
                    (> (getf b :blink-toggle-frame) (getf a :blink-toggle-frame))))))))

(defun ready-linked-p (selected ready)
  "True when READY carries SELECTED's :ID and every receipt field except :FRAMES,
with READY's frames not fewer: no toggle decision came between them."
  (and (well-formed-p selected) (well-formed-p ready)
       (same-keys-p selected ready (cons :id (remove :frames *receipt-keys*)))
       (<= (getf selected :frames) (getf ready :frames))))

(defun expected-blink-phase (pause)
  "Phase to capture at PAUSE and whether one is required. Pause 13 gives
(values NIL T), hidden; 14 and 15 give (values T T); others (values NIL NIL)."
  (case pause (13 (values nil t)) ((14 15) (values t t)) (t (values nil nil))))

(defun validate-blink-selection (polls wanted target)
  "Replay raw selection POLLS, oldest first, for WANTED phase NIL (hidden), T
(visible) or :ANY at TARGET bytes received. Return the selected status and NIL, or
NIL and the errors. Every poll is well formed and visible, and each consecutive
pair satisfies PRECEDES-OR-EQUAL-P within the first poll's source. A poll is fresh
when its toggles exceed the first poll's. The selection is the first fresh poll
with the wanted phase and must be the last poll. The unchanged first generation may
carry the previous epoch's toggle and is never selected; a fresh poll must toggle
inside the epoch. Errors are exactly (:INCOMPLETE) when valid with no selection."
  (flet ((fail (&rest errors) (return-from validate-blink-selection (values nil errors))))
    (unless (member wanted '(nil t :any)) (fail :wanted))
    (unless (typep target '(integer 0)) (fail :target))
    (unless (and (proper-list-p polls) polls) (fail :polls-malformed))
    (loop for poll in polls for i from 0
          unless (well-formed-p poll) do (fail (list :malformed i)))
    (let* ((first (first polls)) (initial (getf first :blink-toggles)) (last (1- (length polls))))
      (unless (eql target (getf first :bytes-received)) (fail :target-mismatch))
      (loop for (a b) on polls for i from 1 while b
            unless (same-keys-p first b *source-keys*) do (fail (list :source-changed i))
            unless (precedes-or-equal-p a b) do (fail (list :order i)))
      (loop for poll in polls for i from 0
            for fresh = (> (getf poll :blink-toggles) initial)
            unless (eq t (getf poll :blink-visible)) do (fail (list :not-visible i))
            when (and fresh (not (blink-capture-ready-p poll poll poll)))
              do (fail (list :stale-toggle i))
            when (and fresh (or (eq wanted :any) (eq wanted (getf poll :blink-phase))))
              do (if (= i last)
                     (return-from validate-blink-selection (values poll nil))
                     (fail (list :appended i))))
      (fail :incomplete))))

(defun blink-selection-record (pause phase target polls)
  "The stored selection record: raw POLLS only, no stored result."
  (list :kind :blink-selection :version 1 :pause pause :phase phase :target target
        :polls polls))

(defun validate-selection-record (record pause ready target)
  "Replay selection RECORD for capture PAUSE against the capture's original READY
and the request TARGET. The phase must be PAUSE's expected phase and the replayed
selection must be READY-LINKED-P to READY. Return the selected status and NIL, or
NIL and the errors."
  (flet ((fail (&rest errors) (return-from validate-selection-record (values nil errors))))
    (unless (and (plist-p record) (equal (record-keys record) *selection-record-keys*))
      (fail :selection-keys))
    (unless (and (eq :blink-selection (getf record :kind)) (eql 1 (getf record :version)))
      (fail :selection-kind))
    (unless (eql pause (getf record :pause)) (fail :pause-mismatch))
    (multiple-value-bind (phase required) (expected-blink-phase pause)
      (unless (and required (eq phase (getf record :phase))) (fail :phase-mismatch))
      (unless (eql target (getf record :target)) (fail :target-mismatch))
      (unless (well-formed-p ready) (fail :ready-malformed))
      (multiple-value-bind (selected errors)
          (validate-blink-selection (getf record :polls) phase target)
        (when errors (fail (cons :selection errors)))
        (unless (ready-linked-p selected ready) (fail :ready-not-linked))
        (values selected nil)))))

(defun chain-report (statuses)
  "VALIDATE-BLINK-CHAIN's report once STATUSES is a finite non-empty list of
well-formed statuses; otherwise an invalid report with error (:STATUSES-MALFORMED)."
  (if (and (proper-list-p statuses) statuses (every #'well-formed-p statuses))
      (validate-blink-chain statuses)
      (list :valid nil :errors '((:statuses-malformed)))))
