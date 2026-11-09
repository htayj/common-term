;;;; Keyboard timing consumer. Timing means the order and process-clock rules
;;;; that CHECK-EVENTS already enforces (strict :seq, monotonic :time on one
;;;; process clock, replies 0..5 clock units after their query, nonoverlapping
;;;; native key brackets without driver bytes, exact per-key bytes), plus the
;;;; ordering below and CAPTURE-ERRORS frame proof for every original capture.
;;;; It is not a wall-clock latency bound and says nothing about pixels. Bracket
;;;; bytes are accepted PTY writes; they do not prove that vttest consumed each
;;;; key in one read (its instr reads one octet, then what is available ~0.1s on).
(in-package #:vttest-keyboard-driver)

;; Must equal LISPTERM_VTTEST_SCALE in vttest-keyboard-launch.lisp until one
;; manifest scale reaches both sites.
(defparameter +timing-scale+ 1)

(defun capture-expectations (label)
  "Map each original capture id to (KEY KEY-PHASE CAPTURE-PHASE)."
  (destructuring-bind (phases keys) (subseq (case-of label) 2 4)
    (append (loop for p below phases
                  append (loop for k in keys
                               collect (list (format nil "keys~d-~a" p (string-downcase k))
                                             k (format nil "keys-~d" p) (format nil "keys-~d" p))))
            (list (list "pause01" "Tab" (format nil "keys-~d" (1- phases)) "pause-1")))))

(defun capture-order-errors (label events)
  "Each original capture follows the :key-end of its own key in its own phase,
with no key bracket, driver input or other capture in between, so it also
precedes the next :key-begin. The final hold follows the last TAB. Names are
owned by LABEL, numbered increasingly, and form exactly CAPTURE-IDS in order."
  (when (eq events :malformed) (return-from capture-order-errors '(:malformed-events)))
  (let ((wanted (capture-expectations label)) (last-end nil) (seen nil) (number -1) (errors nil))
    (dolist (e events)
      (case (getf e :kind)
        (:key-end (setf last-end e))
        ((:key-begin :driver) (setf last-end nil))
        (:capture
         (let* ((name (getf e :name)) (id (capture-id label name))
                (spec (cdr (assoc id wanted :test #'equal)))
                (n (and id (parse-integer name :end (position #\- name)))))
           (push id seen)
           (unless (and spec last-end (> n number)
                        (equal (getf last-end :key) (first spec))
                        (equal (getf last-end :phase) (second spec))
                        (equal (getf e :phase) (third spec)))
             (push (list :capture-order name) errors))
           (when n (setf number n))
           (setf last-end nil)))))
    (unless (equal (reverse seen) (capture-ids label))
      (push :capture-inventory errors))
    (nreverse errors)))

(defun capture-proof-paths (label name)
  (append (mapcar (lambda (s) (concatenate 'string name s))
                  '(".status.sexp" ".barrier.sexp" ".final-status.sexp" ".external.sexp" ".png"))
          (when (equal (capture-id label name) "pause01")
            (list (concatenate 'string name ".grid.txt")))))

(defun transcript-output (label dir)
  "LABEL's raw transcript in the CAPTURE-ERRORS OUTPUT form, one character per
octet (CODE-CHAR of each byte). An unreadable transcript yields the signalled
condition instead, which CONTEXT-TIMING reports only where it needs OUTPUT."
  (handler-case (map 'string #'code-char
                     (vttest-cursor-driver::bytes (artifact dir label ".transcript.raw")))
    (error (e) e)))

(defun context-timing (label dir events output scale &key (barriers nil barriers-p))
  "Timing over supplied EVENTS (the EVENTS-OF form, :MALFORMED included) and
OUTPUT (the TRANSCRIPT-OUTPUT form). The caller has established CASE-BYTES :PASS,
which includes CHECK-EVENTS. Supplied EVENTS, OUTPUT and BARRIERS (an alist of
capture name to barrier plist) are not reread; under DIR only the remaining
capture proof files are checked and read. When BARRIERS is supplied, a capture
without an entry is unverified. Missing proof files are unverified; wrong ones
fail."
  (handler-case
      (let ((order (and dir (capture-order-errors label events))))
        (cond ((null dir) (values :unverified '(:no-run-directory)))
              (order (values :fail order))
              ((notevery (lambda (e)
                           (or (not (eq (getf e :kind) :capture))
                               (every (lambda (p) (present-p (artifact dir p "")))
                                      (capture-proof-paths label (getf e :name)))))
                         events)
               (values :unverified '(:missing-capture-proof)))
              ((and barriers-p
                    (notevery (lambda (e)
                                (or (not (eq (getf e :kind) :capture))
                                    (assoc (getf e :name) barriers :test #'equal)))
                              events))
               (values :unverified '(:missing-barrier)))
              ((typep output 'condition) (values :fail (list (princ-to-string output))))
              ((not (stringp output)) (values :fail '(:timing-output)))
              (t (let ((errors (if barriers-p
                                   (vttest-check::capture-errors label output events dir scale
                                                                 :barriers barriers)
                                   (vttest-check::capture-errors label output events dir scale))))
                   (if errors (values :fail errors) (values :pass nil))))))
    (error (e) (values :fail (list (princ-to-string e))))))

(defun case-timing (label dir bytes scale)
  "BYTES is the CASE-BYTES result for the same LABEL and DIR; its :PASS already
includes CHECK-EVENTS. Reads events and transcript once for CONTEXT-TIMING."
  (handler-case
      (cond ((null dir) (values :unverified '(:no-run-directory)))
            ((not (eq bytes :pass))
             (values (if (eq bytes :fail) :fail :unverified) (list :bytes bytes)))
            (t (context-timing label dir (events-of label dir)
                               (transcript-output label dir) scale)))
    (error (e) (values :fail (list (princ-to-string e))))))

(defun linked-timing (identity timing)
  "Timing passes only with the complete linked original inventory."
  (cond ((or (eq timing :fail) (eq identity :fail)) :fail)
        ((and (eq timing :pass) (eq identity :pass)) :pass)
        (t :unverified)))

(defun timing-status (identity check original timing)
  "Receipt claim for one capture against the recomputed case TIMING."
  (let ((linked (linked-timing identity timing)))
    (cond ((eq linked :fail) :fail)
          ((null original) :unverified)
          ((null check) :unverified)
          ((not (equal (getf check :name) original)) :fail)
          ((eq (getf check :timing) :fail) :fail)
          ((and (eq (getf check :timing) :pass) (eq linked :pass)) :pass)
          (t :unverified))))

(defun timing-aggregate (checks)
  (cond ((some (lambda (k) (eq (getf k :timing) :fail)) checks) :fail)
        ((and (= (length checks) (loop for (label) in +cases+ sum (length (capture-ids label))))
              (every (lambda (k) (eq (getf k :timing) :pass)) checks)) :pass)
        (t :unverified)))
