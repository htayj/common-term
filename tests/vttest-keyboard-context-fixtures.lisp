;;;; Fixtures for vttest-keyboard-context-tests.lisp: function stubs that always
;;;; restore, counted loader reads and barrier-read guards over MOCK-PROOFS data.
;;;; Synthetic recorder data only; never native or pixel proof.
(in-package :vttest-keyboard-driver-tests)

(defparameter *context-stubbed*
  '(k::events-of k::recorder-data vttest-cursor-driver::bytes c:read-value)
  "Every function these controls rebind.")

(defun call-with-stubs (bindings thunk)
  "Install (SYMBOL . FUNCTION) BINDINGS around THUNK; always restore the old ones."
  (let ((old (mapcar (lambda (b) (cons (car b) (symbol-function (car b)))) bindings)))
    (unwind-protect
         (progn (dolist (b bindings) (setf (symbol-function (car b)) (cdr b)))
                (funcall thunk))
      (dolist (o old) (setf (symbol-function (car o)) (cdr o))))))

(defun fresh-dir () (c::cat (mock-proofs (recorder)) "recorder/"))
(defun other-label (label)
  (if (equal label "keyboard-cursor") "keyboard-keypad" "keyboard-cursor"))
(defun capture-names (events)
  (loop for e in events when (eq (getf e :kind) :capture) collect (getf e :name)))

(defun reason-p (key reasons)
  "KEY heads one reason, directly or as the head of the reason's own head."
  (some (lambda (r)
          (let ((head (if (consp r) (first r) r)))
            (eq key (if (consp head) (first head) head))))
        reasons))

(defun expect-load (dir label state key tag ctx-p)
  (multiple-value-bind (ctx s reasons) (k::load-case-context label dir)
    (check (and (eq s state) (eq (and ctx t) ctx-p) (or (null key) (reason-p key reasons)))
           (list label :context tag))
    ctx))

(defun counted-load (label dir)
  "Load once through counting wrappers with *PROOF-TRACE* bound to NIL. Values:
context, state, BYTES calls per path, RECORDER-DATA result per path, the
EVENTS-OF result, and the number of EVENTS-OF calls."
  (let ((reads (make-hash-table :test #'equal)) (data (make-hash-table :test #'equal))
        (calls 0) (events nil) (bytes #'vttest-cursor-driver::bytes)
        (recorder-data #'k::recorder-data) (events-of #'k::events-of))
    (multiple-value-bind (ctx state)
        (call-with-stubs
         (list (cons 'vttest-cursor-driver::bytes
                     (lambda (path) (incf (gethash path reads 0)) (funcall bytes path)))
               (cons 'k::recorder-data
                     (lambda (path &optional many)
                       (setf (gethash path data) (funcall recorder-data path many))))
               (cons 'k::events-of
                     (lambda (l d) (incf calls) (setf events (funcall events-of l d)))))
         (lambda ()
           (let ((k::*proof-trace* nil))
             (multiple-value-prog1 (k::load-case-context label dir)
               (check (null k::*proof-trace*) (list label :loader-adds-no-trace-step))))))
      (values ctx state reads data events calls))))

(defun edit-first-capture (dir label fn)
  "Replace LABEL's first capture event by the events FN returns for a copy of it."
  (let* ((events (k::events-of label dir))
         (i (position :capture events :key (lambda (e) (getf e :kind)))))
    (save-events (c::cat dir label ".events.sexp")
                 (append (subseq events 0 i) (funcall fn (copy-list (nth i events)))
                         (nthcdr (1+ i) events)))))

(defun rename-first (dir label name)
  (edit-first-capture dir label (lambda (e) (setf (getf e :name) name) (list e))))

(defun mutate (label fn)
  "Fresh mock proofs; FN gets the recorder directory and LABEL's first capture name."
  (let ((dir (fresh-dir)))
    (funcall fn dir (first (capture-names (k::events-of label dir))))
    dir))

(defun timing-without-barrier-reads (label dir events output scale &rest keys)
  "CONTEXT-TIMING where reading any barrier file signals; third value is T after one."
  (let ((touched nil))
    (flet ((guarded (symbol)
             (let ((real (symbol-function symbol)))
               (cons symbol (lambda (path &rest args)
                              (when (search ".barrier.sexp" (namestring path))
                                (setf touched t) (error "Unexpected barrier read ~a" path))
                              (apply real path args))))))
      (multiple-value-bind (state reasons)
          (call-with-stubs (mapcar #'guarded (rest *context-stubbed*))
                           (lambda () (apply #'k::context-timing label dir events output scale keys)))
        (values state reasons touched)))))
