;;;; One read of a keyboard case's events, transcript and capture barriers.
;;;; Load after vttest-keyboard-driver.lisp (package, CASE-OF, CAPTURE-IDS, and
;;;; through it vttest-keyboard-artifacts.lisp: PRESENT-P, RECORDER-DATA,
;;;; EVENTS-OF, CAPTURE-ID; native-contract: ARTIFACT) and
;;;; vttest-keyboard-prerequisites.lisp (PREREQ-PLIST-P). No DA, font, decoder,
;;;; row, timing or pixel checks happen here.
(in-package #:vttest-keyboard-driver)

(defstruct case-context label dir events output barriers oracle-octets)

(defparameter *proof-trace* :disabled
  "(STEP . CTX) records, newest first, or :DISABLED. Bind to NIL to collect.")

(defun record-proof-step (step ctx)
  (unless (eq *proof-trace* :disabled) (push (cons step ctx) *proof-trace*))
  ctx)

(defun context-name-p (name)
  (and (stringp name) (plusp (length name))
       (every (lambda (c) (or (char<= #\a c #\z) (digit-char-p c) (find c "-_"))) name)))

(defun load-case-context (label dir)
  "Read LABEL's events, <label>.transcript.raw and each capture's barrier once.
Return the context or NIL, :PASS, :FAIL or :UNVERIFIED, and at most 8 reasons.
OUTPUT is the transcript as one character per octet (CODE-CHAR), as CASE-BYTES
and CASE-TIMING build it. ORACLE-OCTETS starts as NIL; only CASE-PROOF sets it,
after the full font and decoder prerequisites pass.
BARRIERS is an alist (capture-name . plist) in event order. Absent input is
unverified; unsafe or malformed input fails. The context exists only when
events and transcript loaded and nothing failed."
  (let ((reasons '()) (state :pass) (events nil) (octets nil) (barriers '()))
    (labels ((note (new reason)
               (when (< (length reasons) 8) (push reason reasons))
               (when (or (eq new :fail) (eq state :pass)) (setf state new)))
             (finish ()
               (let ((ctx (and (not (eq state :fail)) (consp events) octets
                               (make-case-context
                                :label label :dir dir :events events
                                :output (map 'string #'code-char octets)
                                :barriers (reverse barriers)))))
                 (return-from load-case-context (values ctx state (reverse reasons)))))
             (guard (reason thunk)
               "Value of THUNK and T, or NIL and NIL after noting a failure."
               (handler-case (values (funcall thunk) t)
                 (error (e)
                   (let ((text (princ-to-string e)))
                     (note :fail (list reason (subseq text 0 (min 200 (length text))))))
                   (values nil nil)))))
      (unless (and (stringp label) (case-of label)) (note :fail :unknown-label) (finish))
      (unless dir (note :unverified :no-run-directory) (finish))
      (unless (and (stringp dir) (plusp (length dir))
                   (char= (char dir (1- (length dir))) #\/))
        (note :fail :run-directory) (finish))
      (setf events (events-of label dir))
      (cond ((eq events :malformed) (setf events nil) (note :fail :malformed-events) (finish))
            ((null events) (note :unverified :missing-events)))
      (multiple-value-bind (value ok)
          (guard :unsafe-transcript
                 (lambda () (let ((path (artifact dir label ".transcript.raw")))
                              (when (present-p path) (vttest-cursor-driver::bytes path)))))
        (setf octets value)
        (when (and ok (null value)) (note :unverified :missing-transcript)))
      (when (eq state :fail) (finish))
      (let ((wanted (capture-ids label)) (seen '()))
        (dolist (e events)
          (when (eq (getf e :kind) :capture)
            (let ((name (getf e :name)))
              (cond ((not (and (context-name-p name)
                               (member (guard :capture-name (lambda () (capture-id label name)))
                                       wanted :test #'equal)))
                     (note :fail (list :unowned-capture name)))
                    ((member name seen :test #'equal)
                     (note :fail (list :duplicate-capture name)))
                    (t
                     (push name seen)
                     (multiple-value-bind (plist ok)
                         (guard (list :unsafe-barrier name)
                                (lambda () (let ((path (artifact dir name ".barrier.sexp")))
                                             (if (present-p path) (recorder-data path) :absent))))
                       (cond ((not ok))
                             ((eq plist :absent) (note :unverified (list :missing-barrier name)))
                             ((not (and (consp plist) (prereq-plist-p plist)))
                              (note :fail (list :malformed-barrier name)))
                             (t (push (cons name plist) barriers))))))))))
      (finish))))
