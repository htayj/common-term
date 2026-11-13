(in-package #:lispterm.gpu.tests)

(defun expect-error (function &optional (type 'error))
  (check (handler-case (progn (funcall function) nil)
           (error (condition) (typep condition type)))
         "Expected a condition of type ~A" type))

(defun foundation-checks ()
  (expect-error (lambda () (lispterm.gpu:unavailable 'foundation-check))
                'lispterm.gpu:gpu-unavailable)
  ;; Isolate dispatch controls from all real registrations.
  (let ((*checks* (make-hash-table :test #'equal)))
    (expect-error (lambda () (run-check "gpu-probe")))
    (expect-error (lambda () (run-check "unknown")))
    (expect-error (lambda () (register-check "unknown" (constantly t))))
    (register-check "gpu-probe" (constantly nil))
    (expect-error (lambda () (run-check "gpu-probe"))))
  (dolist (path '("src/gpu/new.lisp" "lispterm.asd" "docs/gpu.org"))
    (check (allowed-authored-path-p path nil) "Rejected Lisp or documentation: ~A" path))
  (dolist (path '("src/gpu/new.c" "src/gpu/new.glsl" "src/gpu/new.py"
                  "tests/new.sh" "scripts/new.nix" "scripts/sbcl"))
    (check (not (allowed-authored-path-p path nil)) "Allowed foreign/new glue: ~A" path))
  (check (allowed-authored-path-p "tests/verify.sh" t) "Rejected existing gate wiring")
  (check (not (allowed-authored-path-p "scripts/unrelated.sh" t)) "Allowed unrelated glue")
  t)

(register-check "gpu-foundation" #'foundation-checks)
