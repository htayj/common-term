(in-package #:lispterm.gpu)

(defun shader-form (stage)
  "Return the authored Lisp form for :VERTEX or :FRAGMENT. Unknown stages are errors."
  (unless (member stage '(:vertex :fragment)) (error "Unknown shader stage ~S" stage))
  (unavailable 'shader-form))
