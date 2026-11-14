(in-package #:lispterm.gpu.tests)

(defun expect-error (function &optional (type 'error))
  (check (handler-case (progn (funcall function) nil)
           (error (condition) (typep condition type)))
         "Expected a condition of type ~A" type))

(defun check-driver-call-protection ()
  (let ((modes (sb-int:get-floating-point-modes))
        (interrupts sb-sys:*interrupts-enabled*))
    (check (eq :returned
               (lispterm.gpu::with-driver-call
                 (check (null (getf (sb-int:get-floating-point-modes) :traps))
                        "Driver call left floating-point traps enabled")
                 (check (not sb-sys:*interrupts-enabled*) "Driver call is interruptible")
                 :returned))
           "Driver call lost its return value")
    (expect-error (lambda () (lispterm.gpu::with-driver-call (error "Driver unwind control"))))
    (check (equal modes (sb-int:get-floating-point-modes)) "Driver call did not restore FP modes")
    (check (eq interrupts sb-sys:*interrupts-enabled*) "Driver call did not restore interrupt state"))
  t)

(defun foundation-checks ()
  (check-driver-call-protection)
  (expect-error (lambda () (lispterm.gpu:unavailable 'foundation-check))
                'lispterm.gpu:gpu-unavailable)
  ;; Isolate dispatch controls from all real registrations.
  (let ((*checks* (make-hash-table :test #'equal)))
    (dolist (name '("gpu-probe" "gpu-preservation" "gpu-fixtures" "gpu-first-frame"
                    "gpu-x11" "gpu-wayland" "gpu-lifecycle"
                    "gpu-dma-release" "gpu-wayland-first-resize"))
      (expect-error (lambda () (run-check name))))
    (expect-error (lambda () (run-check "unknown")))
    (expect-error (lambda () (register-check "unknown" (constantly t))))
    (register-check "gpu-probe" (constantly nil))
    (expect-error (lambda () (run-check "gpu-probe"))))
  (dolist (path '("src/gpu/new.lisp" "lispterm.asd" "docs/gpu.org"))
    (check (allowed-authored-path-p path nil) "Rejected Lisp or documentation: ~A" path))
  (dolist (path '("src/gpu/new.c" "src/gpu/new.glsl" "src/gpu/new.py"
                  "tests/new.sh" "scripts/new.nix" "scripts/sbcl"))
    (check (not (allowed-authored-path-p path nil)) "Allowed foreign/new glue: ~A" path))
  (dolist (path '("tests/verify.sh" "tests/fastfetch-gate.sh"
                  "tests/wayland-gate.sh" "scripts/autolith-runtime"))
    (check (allowed-authored-path-p path t) "Rejected existing gate/wrapper wiring: ~A" path)
    (check (not (allowed-authored-path-p path nil)) "Allowed new glue: ~A" path))
  (check (not (allowed-authored-path-p "scripts/unrelated.sh" t)) "Allowed unrelated glue")
  t)

(register-check "gpu-foundation" #'foundation-checks)
