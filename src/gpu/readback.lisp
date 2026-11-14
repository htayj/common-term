(in-package #:lispterm.gpu)

(defgeneric renderer-readback (renderer)
  (:documentation "Draw pending state and return top-left-origin row-major UB32 #xAARRGGBB.
Readback is presentation/capture output, never a glyph-composition input."))


(defmethod renderer-readback ((r gpu-renderer))
  (renderer-owner r)
  (renderer-draw r)
  (let* ((width (renderer-width r)) (height (renderer-height r))
         (bytes (make-array (* width height 4) :element-type '(unsigned-byte 8)))
         (pixels (make-array (* width height) :element-type '(unsigned-byte 32)))
         (start (get-internal-real-time)))
    (gl-bind-framebuffer #x8D40 (renderer-fbo r))
    (gl-pixel-store-i #x0D05 1)
    (cffi:with-pointer-to-vector-data (p bytes)
      (gl-read-pixels 0 0 width height #x1908 #x1401 p))
    (gl-check 'renderer-readback)
    (dotimes (y height)
      (dotimes (x width)
        (let ((offset (* 4 (+ x (* width (- height y 1))))))
          (setf (aref pixels (+ x (* width y)))
                (logior (ash (aref bytes (+ offset 3)) 24)
                        (ash (aref bytes offset) 16)
                        (ash (aref bytes (+ offset 1)) 8)
                        (aref bytes (+ offset 2)))))))
    (renderer-count r :readbacks)
    (renderer-count r :readback-bytes (length bytes))
    (renderer-count r :readback-ticks (- (get-internal-real-time) start))
    pixels))

(export 'renderer-copy-target)

(defun renderer-copy-target (renderer target-fbo width height &key invert-y)
  "Copy pending GPU pixels to a caller-owned complete framebuffer in this context.
Dimensions must match the renderer exactly. INVERT-Y reverses destination rows.
Restore read/draw framebuffer bindings and scissor state even on failure. No readback."
  (renderer-owner renderer)
  (instance-unsigned target-fbo #xffffffff :target-framebuffer)
  (unless (and (plusp target-fbo) (/= target-fbo (renderer-fbo renderer))
               (eql width (renderer-width renderer)) (eql height (renderer-height renderer)))
    (error "GPU copy requires a distinct framebuffer and exact renderer dimensions"))
  (sb-sys:without-interrupts
    (gl-check 'before-renderer-copy-target)
    (let ((read (gl-integer #x8CAA)) (draw (gl-integer #x8CA6))
          (scissor (gl-integer #x0C11)) (box (gl-integers #x0C10 4)))
      (unwind-protect
           (progn
             (renderer-draw renderer)
             (gl-disable #x0C11)
             (gl-bind-framebuffer #x8CA8 (renderer-fbo renderer))
             (gl-bind-framebuffer #x8CA9 target-fbo)
             (gl-check 'renderer-copy-bind)
             (unless (= (gl-check-framebuffer-status #x8CA9) #x8CD5)
               (unavailable 'renderer-copy-target "Incomplete destination framebuffer"))
             (gl-blit-framebuffer 0 0 width height
                                  0 (if invert-y height 0) width (if invert-y 0 height)
                                  #x4000 #x2600)
             (gl-check 'renderer-copy-target)
             (renderer-count renderer :target-copies))
        (gl-bind-framebuffer #x8CA8 read)
        (gl-bind-framebuffer #x8CA9 draw)
        (apply #'gl-scissor box)
        (if (zerop scissor) (gl-disable #x0C11) (gl-enable #x0C11))
        (gl-check 'renderer-copy-restore))))
  renderer)
