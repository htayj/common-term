(in-package #:lispterm.gpu.tests)

(defun parity-display (width height)
  (let ((d (lispterm.display::make-display
            :renderer-request :cpu
            :font (lispterm.display::load-font nil 16) :owner sb-thread:*current-thread*)))
    (lispterm.display::resize-display d width height)
    d))

(defun parity-write (cpu renderer column row cell)
  "Independent reference is the existing CPU rasterizer, not the shader interpreter."
  (lispterm.display::render-cpu-cell cpu row column cell)
  (multiple-value-bind (foreground background) (lispterm.display::cell-colors cpu row column cell)
    (let* ((glyph (lispterm.display::glyph-bitmap (lispterm.display::display-font cpu)
                                                (lispterm.display::cell-char cell)))
           (mask (map '(vector (unsigned-byte 8)) (lambda (x) (if (zerop x) 0 255)) glyph)))
      (lispterm.gpu:renderer-write-cell
       renderer column row :glyph-key (list (lispterm.display::cell-char cell)
                                            (lispterm.display::cell-bold cell)
                                            (lispterm.display::cell-width cell))
       :mask mask :foreground foreground :background background
       :bold (lispterm.display::cell-bold cell) :underline (lispterm.display::cell-underline cell)
       :hidden (and (lispterm.display::cell-blink cell) (not (lispterm.display::display-blink-phase cpu)))
       :span (lispterm.display::cell-width cell) :line-size (lispterm.display::cell-line-size cell)))))

(defun assert-parity (cpu renderer label)
  (let* ((expected (lispterm.display::display-framebuffer cpu))
         (actual (lispterm.gpu:renderer-readback renderer))
         (difference (mismatch expected actual)))
    (when difference
      (error "~A pixel ~D (~D,~D): CPU ~8,'0X GPU ~8,'0X" label difference
             (mod difference (lispterm.display::display-width cpu))
             (floor difference (lispterm.display::display-width cpu))
             (aref expected difference) (aref actual difference)))
    (format t "~A: ~D exact ARGB pixels~%" label (length actual))
    actual))

(defun gpu-first-frame-check ()
  (let* ((context (lispterm.gpu:open-context))
         (cpu (parity-display 51 35)) (renderer nil))
    (unwind-protect
         (progn
           (lispterm.gpu:require-hardware-context context)
           (let ((font (lispterm.display::display-font cpu)))
             (setf renderer (lispterm.gpu:make-renderer
                             :context context :width 51 :height 35
                             :cell-width (lispterm.display::font-width font)
                             :cell-height (lispterm.display::font-height font))))
           (dotimes (row 2)
             (dotimes (column 6)
               (parity-write cpu renderer column row
                             (lispterm.display::make-cell
                              :char (char "Ag#W0 " column) :fg #xe1ab73 :bg #x123456
                              :bold (oddp column) :underline (= column 3)))))
           ;; A failed upload must leave its glyph pending and the frame retryable.
           (let ((saved (symbol-function 'lispterm.gpu:gl-tex-sub-image-2d)))
             (unwind-protect
                  (progn
                    (setf (symbol-function 'lispterm.gpu:gl-tex-sub-image-2d)
                          (lambda (&rest args) (declare (ignore args))
                            (lispterm.gpu:unavailable 'test-upload "injected transfer failure")))
                    (assert (handler-case (progn (lispterm.gpu:renderer-draw renderer) nil)
                              (lispterm.gpu:gpu-unavailable () t))))
               (setf (symbol-function 'lispterm.gpu:gl-tex-sub-image-2d) saved)))
           (assert (lispterm.gpu:atlas-pending-uploads (lispterm.gpu::renderer-atlas renderer)))
           (assert-parity cpu renderer "FIRST-HARDWARE-FRAME")
           (let ((metrics (lispterm.gpu:renderer-metrics renderer)))
             (assert (= 12 (getf metrics :instances)))
             (assert (= 1 (getf metrics :draw-calls)))
             (assert (plusp (getf metrics :glyph-uploads)))
             (format t "FIRST-FRAME-METRICS ~S~%" metrics))
           t)
      (when renderer (lispterm.gpu:renderer-close renderer))
      (lispterm.gpu:close-context context))))

(register-check "gpu-first-frame" #'gpu-first-frame-check)


(defun parity-gpu-display (cpu renderer)
  (let ((d (lispterm.display::make-display
            :font (lispterm.display::display-font cpu) :owner sb-thread:*current-thread*
            :renderer-request :gpu)))
    (lispterm.display::resize-display d (lispterm.display::display-width cpu)
                                     (lispterm.display::display-height cpu))
    (assert (null (lispterm.display::display-framebuffer-storage d)))
    (setf (lispterm.display::display-renderer-kind d) :gpu
          (lispterm.display::display-renderer-state d) renderer)
    d))

(defun corpus-grid (columns rows iteration line-size)
  (let ((grid (make-array (list rows columns))))
    (dotimes (row rows)
      (dotimes (column columns)
        (let* ((flags (mod (+ iteration column (* 3 row)) 8))
               (wide (and (evenp column) (< (1+ column) columns) (oddp iteration)))
               (continuation (and (oddp column) (oddp iteration))))
          (setf (aref grid row column)
                (lispterm.display::make-cell
                 :char (code-char (+ 33 (mod (+ (* iteration 7) column (* 3 row)) 90)))
                 :width (cond (continuation 0) (wide 2) (t 1))
                 :fg (+ #x201004 (* (mod (+ iteration column) 32) #x030507))
                 :bg (+ #x102030 (* (mod (+ iteration row) 16) #x020103))
                 :bold (logbitp 0 flags) :underline (logbitp 1 flags)
                 :blink (logbitp 2 flags) :inverse (oddp (+ column row))
                 :line-size line-size)))))
    grid))

(defun gpu-offscreen-check ()
  (let ((context (lispterm.gpu:open-context)) (renderer nil) (frames 0) (pixels 0))
    (unwind-protect
         (progn
           (lispterm.gpu:require-hardware-context context)
           (dolist (dimensions '((51 35) (43 33) (8 16) (7 9) (89 48)))
             (destructuring-bind (width height) dimensions
               (let* ((cpu (parity-display width height))
                      (font (lispterm.display::display-font cpu))
                      (cw (lispterm.display::font-width font)) (ch (lispterm.display::font-height font)))
                 (if renderer
                     (lispterm.gpu:renderer-resize renderer width height cw ch)
                     (setf renderer (lispterm.gpu:make-renderer :context context :width width :height height
                                                               :cell-width cw :cell-height ch)))
                 (setf (slot-value (lispterm.gpu::renderer-atlas renderer) 'lispterm.gpu::maximum) 64)
                 (let ((gpu (parity-gpu-display cpu renderer)))
                   (dotimes (iteration 24)
                     (let* ((size (nth (mod (floor iteration 3) 4)
                                       '(:single :double-width :double-height-top :double-height-bottom)))
                            (grid (corpus-grid (lispterm.display::display-columns cpu)
                                               (lispterm.display::display-rows cpu) iteration size))
                            (arguments (list :cursor-row 0 :cursor-col (mod iteration 2)
                                             :cursor-visible (logbitp 0 iteration)
                                             :reverse-screen (logbitp 1 iteration)
                                             :blink-phase (not (logbitp 2 iteration)))))
                       (apply #'lispterm.display::draw-frame cpu grid arguments)
                       (apply #'lispterm.display::draw-frame gpu grid arguments)
                       (assert-parity cpu renderer (format nil "CORPUS ~Dx~D/~D/~A" width height iteration size))
                       (incf frames) (incf pixels (* width height))))))))
           (assert (>= (getf (lispterm.gpu:renderer-metrics renderer) :generations) 5))
           (format t "OFFSCREEN-CORPUS ~D hardware frames / ~D exact pixels~%" frames pixels)
           t)
      (when renderer (lispterm.gpu:renderer-close renderer))
      (lispterm.gpu:close-context context))))

(register-check "gpu-offscreen" #'gpu-offscreen-check)
