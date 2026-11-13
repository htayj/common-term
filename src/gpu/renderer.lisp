(in-package #:lispterm.gpu)

(defun make-renderer (&key context width height cell-width cell-height)
  "Create renderer state on CONTEXT's owner thread, with positive logical pixel sizes."
  (declare (ignore context width height cell-width cell-height))
  (unavailable 'make-renderer))
(defgeneric renderer-resize (renderer width height cell-width cell-height))
(defgeneric renderer-write-cell (renderer column row &key glyph-key mask foreground background
                                                        bold underline hidden span line-size)
  (:documentation "Record one cell. MASK is a row-major UB8 glyph mask, not frame pixels.
Foreground/background are final #xRRGGBB values. See INSTANCE-BUFFER-WRITE."))
(defgeneric renderer-draw (renderer)
  (:documentation "Execute pending GPU uploads and draw before presentation or readback."))
(defgeneric renderer-close (renderer)
  (:documentation "Release GPU resources on the context owner before closing the context."))
(defgeneric renderer-metrics (renderer)
  (:documentation "Return evidence counters; never synthesize successful hardware evidence."))
