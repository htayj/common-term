(defpackage #:lispterm.gpu.tests
  (:use #:cl)
  (:export #:register-check #:run-check #:check #:*root* #:git-output))
(in-package #:lispterm.gpu.tests)

(defparameter *root* (asdf:system-source-directory "lispterm/gpu"))
(defparameter *checks* (make-hash-table :test #'equal))
(defparameter *gate-names*
  '("gpu-shader" "gpu-probe" "gpu-software-control" "gpu-atlas"
    "gpu-offscreen" "gpu-evidence" "gpu-wayland-dmabuf"
    "gpu-language" "gpu-delivery" "gpu-foundation" "gpu-preservation"
    "gpu-fixtures" "gpu-first-frame" "gpu-x11" "gpu-wayland" "gpu-lifecycle"))

(defun check (test format-control &rest args)
  (unless test (error (apply #'format nil format-control args)))
  t)

(defun git-output (root &rest args)
  (uiop:run-program (append (list "git" "-C" (namestring root)) args)
                    :output :string :error-output *error-output*))

(defun register-check (name function)
  "Register only implemented checks. A successful check must return exactly T."
  (check (member name *gate-names* :test #'equal) "Unknown GPU gate ~S" name)
  (check (functionp function) "Not a check function: ~S" function)
  (setf (gethash name *checks*) function))

(defun run-check (name)
  (check (member name *gate-names* :test #'equal) "Unknown GPU gate ~S" name)
  (let ((function (gethash name *checks*)))
    (check function "GPU gate ~A is not implemented" name)
    (check (eq t (funcall function)) "GPU gate ~A did not establish success" name))
  (format t "~A PASS~%" name)
  t)
