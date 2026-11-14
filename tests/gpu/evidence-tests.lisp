(in-package #:lispterm.gpu.tests)

(defun call-with-gpu-evidence-guards (thunk)
  "Observe actual driver operations independently of renderer metrics."
  (let* ((names '(lispterm.gpu:gl-draw-arrays-instanced lispterm.gpu:gl-tex-sub-image-2d
                  lispterm.gpu:gl-tex-image-2d lispterm.gpu:gl-read-pixels
                  lispterm.gpu:gl-buffer-data lispterm.gpu:gl-buffer-sub-data
                  lispterm.display::render-cpu-cell))
         (saved (mapcar (lambda (name) (cons name (symbol-function name))) names))
         (counts (make-hash-table)))
    (unwind-protect
         (progn
           (dolist (entry saved)
             (let ((name (car entry)) (function (cdr entry)))
               (setf (symbol-function name)
                     (lambda (&rest arguments)
                       (incf (gethash name counts 0))
                       (cond ((eq name 'lispterm.display::render-cpu-cell)
                              (error "CPU composition called in GPU evidence region"))
                             ((eq name 'lispterm.gpu:gl-tex-image-2d)
                              (assert (cffi:null-pointer-p (nth 8 arguments))))
                             ((eq name 'lispterm.gpu:gl-tex-sub-image-2d)
                              (assert (= #x1903 (nth 6 arguments)))
                              (assert (= #x1401 (nth 7 arguments)))
                              (incf (gethash :glyph-bytes counts 0) (* (nth 4 arguments) (nth 5 arguments))))
                             ((eq name 'lispterm.gpu:gl-buffer-data)
                              (incf (gethash :instance-bytes counts 0) (second arguments)))
                             ((eq name 'lispterm.gpu:gl-buffer-sub-data)
                              (incf (gethash :instance-bytes counts 0) (third arguments))))
                       (apply function arguments)))))
           (funcall thunk counts))
      (dolist (entry saved) (setf (symbol-function (car entry)) (cdr entry))))))

(defun shader-channel-negative-control (renderer)
  "Change maintained AST channel expressions, relink and observe real changed pixels."
  (let ((form (lispterm.gpu:shader-form :fragment))
        (original (lispterm.gpu::renderer-program renderer)) (alternate 0))
    (labels ((rewrite (node)
               (when (consp node)
                 (if (and (eq (first node) :set) (eq (second node) :o-color))
                     (rotatef (second (third node)) (fourth (third node)))
                     (mapc #'rewrite node)))))
      (rewrite form))
    (unwind-protect
         (progn
           (setf alternate (lispterm.gpu:link-gl-program
                            (lispterm.gpu:emit-shader (lispterm.gpu:shader-form :vertex))
                            (lispterm.gpu:emit-shader form))
                 (lispterm.gpu::renderer-program renderer) alternate
                 (lispterm.gpu::renderer-dirty renderer) t)
           (lispterm.gpu:renderer-readback renderer))
      (setf (lispterm.gpu::renderer-program renderer) original
            (lispterm.gpu::renderer-dirty renderer) t)
      (unless (zerop alternate) (lispterm.gpu:gl-delete-program alternate)))))

(defun gpu-evidence-check ()
  (let* ((context (lispterm.gpu:open-context)) (renderer nil)
         (cpu (parity-display 51 35))
         (grid (corpus-grid 6 2 0 :single))
         (changed-grid (corpus-grid 6 2 0 :single))
         (changed-cpu (parity-display 51 35)))
    (unwind-protect
         (progn
           (lispterm.gpu:require-hardware-context context)
           ;; Compute the reference before installing the no-CPU-composition guard.
           (lispterm.display::draw-frame cpu grid :cursor-row 0 :cursor-col 0 :cursor-visible t)
           (setf (lispterm.display::cell-fg (aref changed-grid 0 0)) #x123abc)
           (lispterm.display::draw-frame changed-cpu changed-grid :cursor-row 0 :cursor-col 0 :cursor-visible t)
           (call-with-gpu-evidence-guards
            (lambda (counts)
              (let ((font (lispterm.display::display-font cpu)))
                (setf renderer (lispterm.gpu:make-renderer
                                :context context :width 51 :height 35
                                :cell-width (lispterm.display::font-width font)
                                :cell-height (lispterm.display::font-height font))))
              (let ((gpu (parity-gpu-display cpu renderer)))
                (lispterm.display::draw-frame gpu grid :cursor-row 0 :cursor-col 0 :cursor-visible t)
                (let* ((first-pixels (assert-parity cpu renderer "EVIDENCE-COLD"))
                       (cold (lispterm.gpu:renderer-metrics renderer)))
                  (assert (plusp (getf cold :glyph-uploads)))
                  (assert (plusp (getf cold :instances)))
                  ;; Cursor restoration redraws real instances while glyphs and records are warm.
                  (lispterm.display::draw-frame gpu grid :cursor-row 0 :cursor-col 0 :cursor-visible t)
                  (assert-parity cpu renderer "EVIDENCE-WARM")
                  (let ((warm (lispterm.gpu:renderer-metrics renderer)))
                    (assert (= (getf cold :glyph-uploads) (getf warm :glyph-uploads)))
                    (assert (= (getf cold :instance-bytes) (getf warm :instance-bytes)))
                    (assert (= (1+ (getf cold :draw-calls)) (getf warm :draw-calls)))
                    (assert (= (1+ (getf cold :readbacks)) (getf warm :readbacks)))
                    (lispterm.display::draw-frame gpu changed-grid :cursor-row 0 :cursor-col 0 :cursor-visible t)
                    (assert-parity changed-cpu renderer "EVIDENCE-ONE-RECORD-CHANGE")
                    (let ((incremental (lispterm.gpu:renderer-metrics renderer)))
                      (assert (= (+ 32 (getf warm :instance-bytes)) (getf incremental :instance-bytes)))
                      (assert (= (getf warm :glyph-uploads) (getf incremental :glyph-uploads))))
                    (let ((changed (shader-channel-negative-control renderer)))
                      (assert (mismatch first-pixels changed)))
                    (assert-parity changed-cpu renderer "EVIDENCE-RESTORED-SHADER")
                    (let ((final (lispterm.gpu:renderer-metrics renderer)))
                      (assert (= (getf final :draw-calls)
                                 (gethash 'lispterm.gpu:gl-draw-arrays-instanced counts 0)))
                      (assert (= (getf final :glyph-uploads)
                                 (gethash 'lispterm.gpu:gl-tex-sub-image-2d counts 0)))
                      (assert (= (getf final :readbacks)
                                 (gethash 'lispterm.gpu:gl-read-pixels counts 0)))
                      (assert (= (getf final :glyph-bytes) (gethash :glyph-bytes counts 0)))
                      (assert (= (getf final :instance-bytes) (gethash :instance-bytes counts 0)))
                      (assert (zerop (gethash 'lispterm.display::render-cpu-cell counts 0)))
                      (format t "MEASURED-GPU-EVIDENCE ~S~%" final)
                      (format t "WARM glyph uploads=0, instance upload bytes=0; actual shader channel negative control changed pixels; no CPU frame composition/upload~%")))))))
           t)
      (when renderer (lispterm.gpu:renderer-close renderer))
      (lispterm.gpu:close-context context))))

(register-check "gpu-evidence" #'gpu-evidence-check)
