;;;; Native blink helpers: live fresh phase selection, toggle chain collection and
;;;; the pause 15 proof. Statuses come from STATUS-FN, time from CLOCK-FN and waits
;;;; from SLEEP-FN, so fixtures need no host. Timing means toggle DECISIONS only.
;;;; Pause 15 chronology: select S0 (:ANY); collect the chain seeded with S0's
;;;; selected status; select S1 (requested phase); READY; the unchanged capture.
(load (merge-pathnames "vttest-screen-blink-selection.lisp" *load-truename*))
(defpackage #:vttest-screen-blink-native
  (:use #:cl #:vttest-screen-blink-timing #:vttest-screen-blink-selection)
  (:export #:select-fresh-phase #:collect-blink-chain #:blink-chain-record
           #:validate-blink-proof #:*chain-pause* #:*chain-record-keys*
           #:blink-refusal #:blink-refusal-reason #:blink-refusal-observations
           ;; Re-exported from VTTEST-SCREEN-BLINK-SELECTION for recorder and checker.
           #:expected-blink-phase #:validate-blink-selection #:ready-linked-p
           #:precedes-or-equal-p #:blink-selection-record #:validate-selection-record))
(in-package #:vttest-screen-blink-native)

(defparameter *chain-pause* 15 "The one pause whose record carries a toggle chain.")
(defparameter *chain-record-keys* '(:kind :version :pause :selection :statuses :report))

(define-condition blink-refusal (error)
  ((reason :initarg :reason :reader blink-refusal-reason)
   (observations :initarg :observations :reader blink-refusal-observations))
  (:report (lambda (c s) (format s "Blink refusal ~S after ~D observations"
                                 (blink-refusal-reason c)
                                 (length (blink-refusal-observations c))))))

(defun refuse (reason polls)
  "Signal BLINK-REFUSAL carrying every raw poll, oldest first (POLLS is newest first)."
  (error 'blink-refusal :reason reason :observations (reverse polls)))

(defun monotonic-seconds () (/ (get-internal-real-time) internal-time-units-per-second))
(defun short-sleep () (sleep 1/50))

(defun select-fresh-phase (status-fn wanted target &key (seconds 10)
                                                    (clock-fn #'monotonic-seconds)
                                                    (sleep-fn #'short-sleep))
  "Poll STATUS-FN until VALIDATE-BLINK-SELECTION over all raw polls selects a
fresh WANTED phase (NIL, T or :ANY) at TARGET bytes. Return the selected status
and the polls, oldest first. Only selects a frame; never captures. Polling
continues only while the errors are exactly (:INCOMPLETE) and CLOCK-FN is before
the deadline; otherwise signal BLINK-REFUSAL with the errors or :DEADLINE."
  (let ((deadline (+ (funcall clock-fn) seconds)) (polls '()))
    (loop
      (push (funcall status-fn) polls)
      (multiple-value-bind (selected errors)
          (validate-blink-selection (reverse polls) wanted target)
        (cond ((null errors) (return (values selected (reverse polls))))
              ((not (equal errors '(:incomplete))) (refuse errors polls))
              ((>= (funcall clock-fn) deadline) (refuse :deadline polls))))
      (funcall sleep-fn))))

(defun collect-blink-chain (status-fn &key seed (seconds 15) (clock-fn #'monotonic-seconds)
                                        (sleep-fn #'short-sleep))
  "Poll STATUS-FN until SEED, when given, and all observations form a valid chain
of four consecutive toggle generations; return the raw statuses, oldest first, and
the report. SEED is S0, the status selected by SELECT-FRESH-PHASE with :ANY. Any
refusal other than too few generations signals BLINK-REFUSAL at once with the
report errors; so does the deadline."
  (let ((deadline (+ (funcall clock-fn) seconds)) (polls (and seed (list seed))))
    (loop
      (push (funcall status-fn) polls)
      (let* ((report (chain-report (reverse polls)))
             (errors (getf report :errors)))
        (cond ((getf report :valid) (return (values (reverse polls) report)))
              ((notevery (lambda (e) (eq (first e) :fewer-than-four-toggle-generations))
                         errors)
               (refuse errors polls))
              ((>= (funcall clock-fn) deadline) (refuse :deadline polls))))
      (funcall sleep-fn))))

(defun blink-chain-record (pause selection-polls statuses)
  "The stored version 2 chain record: S0's raw SELECTION-POLLS, the chain STATUSES
and the :REPORT computed here from STATUSES."
  (list :kind :blink-chain :version 2 :pause pause :selection selection-polls
        :statuses statuses :report (chain-report statuses)))

(defun well-formed-list (x)
  "X when it is a finite list of well-formed statuses, otherwise NIL."
  (and (proper-list-p x) (every #'well-formed-p x) x))

(defun validate-blink-proof (pause chain-record selection-record ready target)
  "Check the pause 15 CHAIN-RECORD and the capture's SELECTION-RECORD against its
original READY status and the request TARGET. The stored report must be a finite
acyclic plist, checked before any comparison, EQUAL to a valid recomputed one. S0
must replay with :ANY and equal the chain's first status; the chain's last status
must PRECEDES-OR-EQUAL-P S1's first poll; S1 must replay via
VALIDATE-SELECTION-RECORD, linked to READY; every status shares READY's source.
READY's fields are read only once it is well formed. Return T or NIL and the refusals."
  (let ((errors '()) (s0 nil) (s1 nil))
    (flet ((need (ok what) (unless ok (push what errors))))
      (need (eql pause *chain-pause*) :not-chain-pause)
      (need (well-formed-p ready) :ready-malformed)
      (need (and (typep target '(integer 0))
                 (or (not (well-formed-p ready)) (eql target (getf ready :bytes-received))))
            :target-mismatch)
      (multiple-value-bind (selected s1-errors)
          (validate-selection-record selection-record pause ready target)
        (setf s1 selected)
        (need selected (cons :s1 s1-errors)))
      (if (not (and (plist-p chain-record)
                    (equal (record-keys chain-record) *chain-record-keys*)))
          (push :record-keys errors)
          (let* ((statuses (getf chain-record :statuses))
                 (report (chain-report statuses))
                 (stored (getf chain-record :report))
                 (stored-ok (and (vttest-screen-blink-selection::finite-tree-p stored)
                                 (plist-p stored))))
            (need stored-ok :report-malformed)
            (need (eq :blink-chain (getf chain-record :kind)) :kind)
            (need (eql 2 (getf chain-record :version)) :version)
            (need (eql pause (getf chain-record :pause)) :pause-mismatch)
            (multiple-value-bind (selected s0-errors)
                (validate-blink-selection (getf chain-record :selection) :any target)
              (setf s0 selected)
              (need selected (cons :s0 s0-errors)))
            (need (getf report :valid) :chain-invalid)
            (when stored-ok (need (equal report stored) :report-not-recomputed))
            (when (and s0 (getf report :valid))
              (need (equal (first statuses) s0) :chain-first-not-s0))
            (when (and s1 (getf report :valid))
              (need (precedes-or-equal-p (car (last statuses))
                                         (first (getf selection-record :polls)))
                    :chain-not-before-s1))
            (when (and (getf report :valid) (well-formed-p ready))
              (let ((evidence (append (well-formed-list (getf chain-record :selection)) statuses
                                      (well-formed-list (getf selection-record :polls)))))
                (dolist (key *source-keys*)
                  (need (every (lambda (s) (equal (getf s key) (getf ready key))) evidence)
                        (list :ready-mismatch key))))))))
    (values (null errors) (nreverse errors))))
