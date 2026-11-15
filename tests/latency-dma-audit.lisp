;;;; Native target-buffer authentication, excluded from timing runs.
(in-package #:lispterm.latency.raw)

(defparameter *skip-audit-copy* nil)
(defparameter *audit-renderers* nil)

(defun dma-audit (slots table)
  "Read only token rectangles from the DMA target about to be attached.
The DMA copy inverts Y, so target GL rows have the terminal's top origin."
  (lambda (store plist)
    (unless (and (eq :dma (getf plist :path))
                 (typep store 'lispterm.display.wayland::dma-buffer))
      (error "Native audit requires a DMA target"))
    (let* ((target (lispterm.display.wayland::dma-buffer-target store))
           (fbo (lispterm.gpu:dma-target-framebuffer target))
           (renderer (gethash fbo table)))
      (unless renderer (error "Unobserved DMA copy target ~D" fbo))
      (lispterm.gpu::renderer-owner renderer)
      (let* ((cw (lispterm.gpu::renderer-cw renderer))
             (ch (lispterm.gpu::renderer-ch renderer))
             (x (* +token-column+ cw)) (width (* +token-length+ cw))
             (bytes (make-array (* width ch 4) :element-type '(unsigned-byte 8)))
             (read (lispterm.gpu::gl-integer #x8CAA))
             (pack (lispterm.gpu::gl-integer #x0D05)))
        (unwind-protect
             (progn
               (lispterm.gpu::gl-bind-framebuffer #x8CA8 fbo)
               (lispterm.gpu::gl-pixel-store-i #x0D05 1)
               (unless (= (lispterm.gpu::gl-check-framebuffer-status #x8CA8) #x8CD5)
                 (error "Incomplete audit framebuffer"))
               (cffi:with-pointer-to-vector-data (pointer bytes)
                 (loop for row from 1 to slots
                       for y = (* row ch)
                       when (and (<= (+ x width) (getf plist :width))
                                 (<= (+ y ch) (getf plist :height)))
                         collect
                         (progn
                           (lispterm.gpu::gl-read-pixels x y width ch #x1908 #x1401 pointer)
                           (lispterm.gpu::gl-check 'native-dma-token-audit)
                           (cons row (pixel-rectangle-hash pointer width 0 0 width ch))))))
          (lispterm.gpu::gl-bind-framebuffer #x8CA8 read)
          (lispterm.gpu::gl-pixel-store-i #x0D05 pack)
          (lispterm.gpu::gl-check 'native-dma-audit-restore))))))

(defun call-with-dma-copy-audit (function)
  "Intercept the existing copy boundary only within an owned fixture.
Keep the real target identity even when deliberately skipping the copy."
  (let ((original (symbol-function 'lispterm.gpu:renderer-copy-target))
        (*audit-renderers* (make-hash-table)))
    ;; Owner threads do not inherit dynamic bindings. The table and switch are
    ;; global for this exclusively locked fixture, restored at exit.
    (let ((table *audit-renderers*))
      (unwind-protect
           (progn
             (setf (symbol-function 'lispterm.gpu:renderer-copy-target)
                   (lambda (renderer target width height &rest options)
                     (setf (gethash target table) renderer)
                     (if *skip-audit-copy* renderer
                         (apply original renderer target width height options))))
             (funcall function table))
        (setf (symbol-function 'lispterm.gpu:renderer-copy-target) original
              *skip-audit-copy* nil)))))
