;;;; Fixture support for vttest-keyboard-timing-tests.lisp: row-23 output inside each
;;;; recorder key bracket and a pinned-font proof, so CASE-PROOF reaches timing over
;;;; one context. Synthetic recorder data; never native or pixel proof. Row text comes
;;;; from vttest-keyboard-row-output, whose literal controls are its own tests.
(in-package :vttest-keyboard-driver-tests)

(defun row-text (label phase key)
  "Row 23 as vttest writes it for KEY in keys-PHASE: the erase, then the TAB echo
or the key's reverse echo and label redraw."
  (let ((k (position key (fourth (k::case-of label)) :test #'equal)))
    (concatenate 'string (vttest-keyboard-row-output::row-erase)
                 (cond ((equal key "Tab") (vttest-keyboard-row-output::tab-redraw))
                       (k (vttest-keyboard-row-output::key-redraw label phase k))
                       (t (error "No row text for key ~s of ~a" key label))))))

(defun add-row-output (root)
  "Write ROW-TEXT just before each keys-phase :KEY-END and the hold prompt just after
the pause-1 :PHASE event, renumber :SEQ, :TIME, :OUT and :END, and rewrite each
transcript from its :WIRE-OUT bytes. Returns ROOT."
  (let ((dir (c::cat root "recorder/")))
    (dolist (label '("keyboard-cursor" "keyboard-keypad") root)
      (let ((events '()) (out 0) (n 0) (transcript ""))
        (flet ((add (e)
                 (let ((c (copy-list e)))
                   (setf (getf c :seq) (incf n) (getf c :time) (/ n 100) (getf c :out) out)
                   (when (eq (getf c :kind) :wire-out)
                     (setf transcript (c::cat transcript (getf c :bytes))
                           out (length transcript) (getf c :end) out))
                   (push c events))))
          (flet ((row (e text)
                   (add (list :seq 0 :kind :wire-out :phase (getf e :phase) :time 0 :out 0
                              :in (getf e :in) :bytes text :end 0))))
            (dolist (e (k::events-of label dir))
              (let ((phase (getf e :phase)))
                (when (and (eq (getf e :kind) :key-end) (eql 0 (search "keys-" phase)))
                  (row e (row-text label (parse-integer phase :start 5) (getf e :key))))
                (add e)
                (when (and (eq (getf e :kind) :phase) (equal phase "pause-1"))
                  (row e (c::cat (vttest-keyboard-row-output::row-erase)
                                 vttest-keyboard-row-output::+hold+)))))))
        (put-bytes (c::cat dir label ".transcript.raw") transcript)
        (save-events (c::cat dir label ".events.sexp") (nreverse events))))))

(defun timed-root (root)
  "ROOT with row output, then MOCK-PROOFS records whose targets are the new :OUTs."
  (mock-proofs (add-row-output root)))

(defvar *pinned-font* nil "Cached (STATE REASONS OCTETS) of PINNED-FONT-PROOF.")

(defun pinned-font-proof ()
  "Test FONT-PROOF closure: the pinned font through the production LOAD-PROVEN-FONT,
then the production decoder against the oracle for exactly codes 32..126, as
FONT-PROOF does after its loaded-font evidence, which needs a live run."
  (values-list
   (or *pinned-font*
       (setf *pinned-font*
             (multiple-value-bind (font octets)
                 (k::load-proven-font (namestring (truename (vttest-keyboard-decoder:font-path))))
               (let ((codes (vttest-keyboard-decoder:compare-codes font octets)))
                 (if (and (equal (mapcar #'car codes) (loop for c from 32 to 126 collect c))
                          (every (lambda (r) (eq :match (cdr r))) codes))
                     (list :pass nil octets)
                     (list :fail (list :decoder-disagrees) nil))))))))

(defun mock-evidence (root)
  "NATIVE-EVIDENCE over ROOT with PINNED-FONT-PROOF and no pixel scratch."
  (k::native-evidence root #'pinned-font-proof nil))
