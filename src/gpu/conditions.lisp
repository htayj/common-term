(in-package #:lispterm.gpu)

(define-condition gpu-unavailable (error)
  ((operation :initarg :operation :reader gpu-operation)
   (detail :initarg :detail :reader gpu-detail))
  (:report (lambda (condition stream)
             (format stream "GPU ~A unavailable: ~A"
                     (gpu-operation condition) (gpu-detail condition)))))

(defun unavailable (operation &optional (detail "implementation pending"))
  (error 'gpu-unavailable :operation operation :detail detail))
