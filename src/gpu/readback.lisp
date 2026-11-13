(in-package #:lispterm.gpu)

(defgeneric renderer-readback (renderer)
  (:documentation "Draw pending state and return top-left-origin row-major UB32 #xAARRGGBB.
Readback is presentation/capture output, never a glyph-composition input."))
