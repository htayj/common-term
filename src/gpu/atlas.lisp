(in-package #:lispterm.gpu)

(defun make-atlas (&key tile-width tile-height capacity max-capacity)
  "Create a CPU-side slot model, not a CPU framebuffer. Limits must be validated."
  (declare (ignore tile-width tile-height capacity max-capacity))
  (unavailable 'make-atlas))
(defgeneric atlas-begin-frame (atlas keys)
  (:documentation "KEYS lists every glyph needed in the new frame. Protect before eviction."))
(defgeneric atlas-ensure-glyph (atlas key mask)
  (:documentation "KEY is immutable EQUAL data (grapheme bold span); MASK is row-major UB8.
Return slot, epoch and upload-needed-p. Never evict a slot referenced this frame."))
(defgeneric atlas-epoch (atlas)
  (:documentation "Monotonic rebuild generation, incremented on eviction or growth."))
