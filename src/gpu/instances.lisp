(in-package #:lispterm.gpu)

(defun make-instance-buffer (&key columns rows)
  "Allocate cell records. Packed byte layout is intentionally not frozen yet."
  (declare (ignore columns rows))
  (unavailable 'make-instance-buffer))
(defgeneric instance-buffer-write (buffer column row &key slot foreground background
                                                       bold underline hidden span line-size)
  (:documentation "Colors are final #xRRGGBB integers; coordinates and slot are nonnegative.
SPAN is 1 or 2. LINE-SIZE uses the display's existing :SINGLE, :DOUBLE-WIDTH,
:DOUBLE-HEIGHT-TOP and :DOUBLE-HEIGHT-BOTTOM keywords."))
(defgeneric instance-buffer-octets (buffer)
  (:documentation "Return specialized UB8 storage; the validated layout belongs to this module."))
