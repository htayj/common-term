(in-package #:lispterm.gpu.tests)

(defun gpu-software-control-check ()
  ;; Explicit device selection, not an inherited LIBGL_ALWAYS_SOFTWARE hint.
  (let ((context (lispterm.gpu:open-context :software t)))
    (unwind-protect
         (progn
           (let ((info (lispterm.gpu:context-info context)))
             (assert (eq t (getf info :software-p)))
             (assert (lispterm.gpu:extension-present-p
                      "EGL_MESA_device_software" (getf info :device-extensions)))
             (assert (not (getf info :device-associated-p)))
             (format t "SOFTWARE-CONTEXT ~S~%" info))
           (assert (assert-offscreen-draw context))
           ;; Only this typed hardware-requirement rejection counts as success.
           ;; An unrelated loader, shader or draw error propagates and fails.
           (assert
            (handler-case (progn (lispterm.gpu:require-hardware-context context) nil)
              (lispterm.gpu:gpu-software-rejected () t))))
      (lispterm.gpu:close-context context)))
  (assert (null lispterm.gpu::*live-contexts*))
  ;; Prove selection does not poison a subsequent mandatory-hardware context.
  (let ((hardware (lispterm.gpu:open-context)))
    (unwind-protect
         (progn
           (assert (lispterm.gpu:require-hardware-context hardware))
           (assert (assert-offscreen-draw hardware)))
      (lispterm.gpu:close-context hardware)))
  (assert (null lispterm.gpu::*live-contexts*))
  (format t "SOFTWARE-CONTROL actual EGL software-device draw passed; hardware policy rejected exactly gpu-software-rejected; subsequent hardware draw passed~%")
  t)

(register-check "gpu-software-control" #'gpu-software-control-check)
