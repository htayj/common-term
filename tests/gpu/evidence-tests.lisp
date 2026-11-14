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

(defun check-renderer-delete-errors (context &optional (modes '(:stale :buffer :program :resize)))
  "Inject real GLES errors, not Lisp throws from the deletion wrapper."
  (dolist (mode modes)
    (let* ((r (lispterm.gpu:make-renderer :context context :width 16 :height 16
                                       :cell-width 8 :cell-height 16))
           (name (if (eq mode :program) 'lispterm.gpu:gl-delete-program
                     (if (eq mode :resize) 'lispterm.gpu:gl-delete-textures
                         'lispterm.gpu:gl-delete-buffers)))
           (saved (symbol-function name))
           (calls 0) (shader 0)
           (old (if (eq mode :program) (lispterm.gpu::renderer-program r)
                    (if (eq mode :resize) (lispterm.gpu::renderer-target r)
                        (lispterm.gpu::renderer-vbo r)))))
      (unwind-protect
           (progn
             (when (eq mode :program)
               (setf shader (lispterm.gpu:gl-create-shader #x8B31)))
             (setf (symbol-function name)
                   (lambda (&rest args)
                     (incf calls)
                     (case mode
                       (:stale (apply saved args))
                       (:program (funcall saved shader)) ; shader is not a program: INVALID_OPERATION
                       (otherwise (funcall saved -1 (second args)))))) ; INVALID_VALUE
             (when (eq mode :stale) (lispterm.gpu:gl-enable #xffffffff))
             (assert (handler-case
                         (progn (if (eq mode :resize)
                                    (lispterm.gpu:renderer-resize r 24 16 8 16)
                                    (lispterm.gpu:renderer-close r)) nil)
                       (lispterm.gpu:gpu-unavailable (e)
                         (assert (search (case mode (:stale "#x500") (:program "#x502")
                                               (otherwise "#x501"))
                                         (lispterm.gpu::gpu-detail e)))
                         t)))
             (assert (not (lispterm.gpu::renderer-closed r)))
             (assert (= old (case mode
                              (:program (lispterm.gpu::renderer-program r))
                              (:resize (lispterm.gpu::renderer-target r))
                              (otherwise (lispterm.gpu::renderer-vbo r)))))
             (when (eq mode :stale) (assert (zerop calls)))
             (when (eq mode :resize)
               (assert (= 16 (lispterm.gpu::renderer-width r)))
               (assert (lispterm.gpu::renderer-pending-textures r))
               (setf (symbol-function name) saved)
               (let ((cpu (parity-display 16 16)))
                 (parity-write cpu r 0 0 (lispterm.display::make-cell :char #\R :fg #xabcdef :bg #x123456))
                 (assert-parity cpu r "FAILED-RESIZE-OLD-TARGET")))
             (format t "REAL-GL-DELETE-ERROR ~A retained ownership; deletion calls ~D~%" mode calls)
             (when (eq mode :buffer)
               (handler-bind
                   ((lispterm.gpu:gpu-unavailable
                      (lambda (e)
                        (assert (find-restart 'lispterm.gpu::retry-renderer-close e))
                        (setf (symbol-function name) saved)
                        (invoke-restart 'lispterm.gpu::retry-renderer-close))))
                 (lispterm.gpu:renderer-close r))
               (format t "RENDERER-CLOSE retry restart released retained objects~%")))
        (setf (symbol-function name) saved)
        (loop repeat 16 until (zerop (lispterm.gpu:gl-get-error)))
        (unless (zerop shader) (lispterm.gpu:gl-delete-shader shader))
        (lispterm.gpu:renderer-close r))
      (assert (lispterm.gpu::renderer-closed r))
      (assert (null (lispterm.gpu::renderer-pending-textures r)))
      (assert (every #'zerop (list (lispterm.gpu::renderer-vbo r)
                                  (lispterm.gpu::renderer-vao r)
                                  (lispterm.gpu::renderer-fbo r)
                                  (lispterm.gpu::renderer-target r)
                                  (lispterm.gpu::renderer-texture r)
                                  (lispterm.gpu::renderer-program r)))))))

(defun check-renderer-partial-initialization (context)
  (dolist (name '(lispterm.gpu:gl-gen-buffers lispterm.gpu:gl-tex-image-2d))
    (let ((saved (symbol-function name))
          (close (symbol-function 'lispterm.gpu:renderer-close)) (partial nil))
      (unwind-protect
           (progn
             (setf (symbol-function name)
                   (lambda (&rest args)
                     (if (eq name 'lispterm.gpu:gl-gen-buffers)
                         (setf (first args) -1)
                         (setf (fourth args) -1))
                     (apply saved args))
                   (symbol-function 'lispterm.gpu:renderer-close)
                   (lambda (r) (setf partial r) (funcall close r)))
             (assert (handler-case
                         (progn (lispterm.gpu:make-renderer :context context :width 16 :height 16
                                                          :cell-width 8 :cell-height 16) nil)
                       (lispterm.gpu:gpu-unavailable () t)))
             (assert partial)
             (assert (lispterm.gpu::renderer-closed partial))
             (assert (null (lispterm.gpu::renderer-pending-textures partial)))
             (dolist (accessor '(lispterm.gpu::renderer-vbo lispterm.gpu::renderer-vao
                                 lispterm.gpu::renderer-fbo lispterm.gpu::renderer-target
                                 lispterm.gpu::renderer-texture lispterm.gpu::renderer-program))
               (assert (zerop (funcall accessor partial))))
             (lispterm.gpu:gl-check 'partial-initialization-control)
             (format t "REAL-GL-PARTIAL-INIT ~A: all owned handles released~%" name))
        (setf (symbol-function name) saved
              (symbol-function 'lispterm.gpu:renderer-close) close)
        (loop repeat 16 until (zerop (lispterm.gpu:gl-get-error)))
        (when (and partial (not (lispterm.gpu::renderer-closed partial)))
          (funcall close partial))))))

(defun check-renderer-copy-target (context)
  (let* ((cpu (parity-display 24 32)) (source nil) (target nil)
         (font (lispterm.display::display-font cpu))
         (cw (lispterm.display::font-width font)) (ch (lispterm.display::font-height font))
         (saved-read (symbol-function 'lispterm.gpu:gl-read-pixels))
         (saved-blit (symbol-function 'lispterm.gpu::gl-blit-framebuffer)))
    (unwind-protect
         (progn
           (setf source (lispterm.gpu:make-renderer :context context :width 24 :height 32
                                                 :cell-width cw :cell-height ch)
                 target (lispterm.gpu:make-renderer :context context :width 24 :height 32
                                                 :cell-width cw :cell-height ch))
           (dotimes (row 2)
             (parity-write cpu source 0 row
                           (lispterm.display::make-cell :char (if (zerop row) #\A #\W)
                                                       :fg #xabcdef :bg (if (zerop row) #x123456 #x654321))))
           (let ((expected (copy-seq (lispterm.display::display-framebuffer cpu)))
                 (fbo (lispterm.gpu::renderer-fbo target)))
             (dolist (invert '(nil t))
               (lispterm.gpu:gl-bind-framebuffer #x8CA8 fbo)
               (lispterm.gpu:gl-bind-framebuffer #x8CA9 (lispterm.gpu::renderer-fbo source))
               (lispterm.gpu:gl-enable #x0C11)
               (lispterm.gpu:gl-scissor 1 2 3 4)
               (unwind-protect
                    (progn
                      (setf (symbol-function 'lispterm.gpu:gl-read-pixels)
                            (lambda (&rest args) (declare (ignore args)) (error "Readback during GPU copy")))
                      (lispterm.gpu:renderer-copy-target source fbo 24 32 :invert-y invert))
                 (setf (symbol-function 'lispterm.gpu:gl-read-pixels) saved-read))
               (assert (= fbo (lispterm.gpu:gl-integer #x8CAA)))
               (assert (= (lispterm.gpu::renderer-fbo source) (lispterm.gpu:gl-integer #x8CA6)))
               (assert (= 1 (lispterm.gpu:gl-integer #x0C11)))
               (assert (equal '(1 2 3 4) (lispterm.gpu:gl-integers #x0C10 4)))
               (setf (lispterm.gpu::renderer-dirty target) nil)
               (let ((pixels (lispterm.gpu:renderer-readback target)))
                 (dotimes (y 32)
                   (dotimes (x 24)
                     (assert (= (aref pixels (+ x (* y 24)))
                                (aref expected (+ x (* (if invert (- 31 y) y) 24))))))))
               (format t "GPU-TARGET-COPY invert=~S: 768 exact pixels; no readback; bindings/scissor restored~%" invert))
             ;; Inject a real invalid blit mask. A failed copy restores bindings
             ;; and must not increment the successful-copy counter.
             (let ((read (lispterm.gpu:gl-integer #x8CAA))
                   (draw (lispterm.gpu:gl-integer #x8CA6)))
               (unwind-protect
                    (progn
                      (setf (symbol-function 'lispterm.gpu::gl-blit-framebuffer)
                            (lambda (&rest args)
                              (setf (nth 8 args) #xffffffff)
                              (apply saved-blit args)))
                      (assert (handler-case
                                  (progn (lispterm.gpu:renderer-copy-target source fbo 24 32) nil)
                                (lispterm.gpu:gpu-unavailable (e)
                                  (assert (search "#x501" (lispterm.gpu::gpu-detail e))) t))))
                 (setf (symbol-function 'lispterm.gpu::gl-blit-framebuffer) saved-blit))
               (assert (= read (lispterm.gpu:gl-integer #x8CAA)))
               (assert (= draw (lispterm.gpu:gl-integer #x8CA6))))
             (assert (= 2 (getf (lispterm.gpu:renderer-metrics source) :target-copies)))
             (assert (zerop (getf (lispterm.gpu:renderer-metrics source) :readbacks 0)))))
      (setf (symbol-function 'lispterm.gpu:gl-read-pixels) saved-read
            (symbol-function 'lispterm.gpu::gl-blit-framebuffer) saved-blit)
      (lispterm.gpu:gl-disable #x0C11)
      (when target (lispterm.gpu:renderer-close target))
      (when source (lispterm.gpu:renderer-close source)))))

(defun gpu-evidence-check ()
  (let* ((context (lispterm.gpu:open-context)) (renderer nil)
         (cpu (parity-display 51 35))
         (grid (corpus-grid 6 2 0 :single))
         (changed-grid (corpus-grid 6 2 0 :single))
         (changed-cpu (parity-display 51 35)))
    (unwind-protect
         (progn
           (lispterm.gpu:require-hardware-context context)
           (check-renderer-delete-errors context)
           (check-renderer-partial-initialization context)
           (check-renderer-copy-target context)
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
