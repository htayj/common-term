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

(defun check-overlapping-cell-writes (context)
  "Rectangular CPU writes preserve uncovered pixels, even across line modes."
  (let* ((cpu (parity-display 48 32))
         (font (lispterm.display::display-font cpu))
         (cw (lispterm.display::font-width font))
         (ch (lispterm.display::font-height font))
         (renderer (lispterm.gpu:make-renderer :context context :width 48 :height 32
                                            :cell-width cw :cell-height ch)))
    (unwind-protect
         (progn
           (dolist (write '((1 0 #\W 2 :single #xabcdef #x123456)
                            (0 0 #\A 2 :single #xfedcba #x654321)
                            (1 0 #\B 1 :single #xeeeeee #x234567)
                            (0 1 #\W 2 :double-width #xabcdef #x123456)
                            (1 1 #\A 1 :single #xfedcba #x654321)
                            (0 1 #\B 1 :double-width #xffffff #x234567)
                            (1 1 #\R 1 :double-width #xabcdef #x123456)
                            (1 1 #\S 1 :single #xfedcba #x654321)))
             (destructuring-bind (column row char span size fg bg) write
               (parity-write cpu renderer column row
                             (lispterm.display::make-cell :char char :width span
                                                         :line-size size :fg fg :bg bg))
               (assert-parity cpu renderer (format nil "OVERLAP ~S" write))))
           ;; Repeated identical overwrites must not accumulate dead fragments.
           (dotimes (i 32)
             (parity-write cpu renderer 0 0
                           (lispterm.display::make-cell :char #\A :width 2
                                                       :fg #xfedcba :bg #x654321)))
           (assert (< (hash-table-count (lispterm.gpu::renderer-cells renderer)) 16))
           (assert-parity cpu renderer "OVERLAP-REPEATED")
           ;; Exercise the public damage cache too: unrelated cached cells are
           ;; not redrawn by DRAW-CELLS and cannot hide lost retained pixels.
           (setf cpu (parity-display 48 32))
           (lispterm.gpu:renderer-resize renderer 48 32 cw ch)
           (let ((gpu (parity-gpu-display cpu renderer)))
             (dolist (column '(1 0))
               (let ((cell (lispterm.display::make-cell :char (if (zerop column) #\A #\W)
                                                      :width 2 :fg #xabcdef :bg #x123456)))
                 (lispterm.display::draw-cells cpu 0 column (vector cell))
                 (lispterm.display::draw-cells gpu 0 column (vector cell))
                 (assert-parity cpu renderer (format nil "DAMAGE-OVERLAP ~D" column))))
             ;; A complete row repaint does cover the old right half, explaining
             ;; why full-grid corpus checks alone did not expose this defect.
             (let ((grid (corpus-grid 6 2 0 :single)))
               (lispterm.display::draw-frame cpu grid)
               (lispterm.display::draw-frame gpu grid)
               (assert-parity cpu renderer "OVERLAP-FULL-ROW-REPAINT"))))
      (lispterm.gpu:renderer-close renderer))))

;;;; Incremental packing controls: counters exclude oracle calls and readback.
(defparameter *packing-keys*
  '(:full-packs :incremental-packs :records-encoded :atlas-resolutions :records-compared
    :position-sorts :instance-upload-calls :instance-bytes :draw-calls :glyph-uploads))

(defun packing-counter (r key) (gethash key (lispterm.gpu::renderer-counters r) 0))
(defun packing-snapshot (r) (mapcar (lambda (key) (packing-counter r key)) *packing-keys*))
(defun packing-deltas (r before)
  (loop for key in *packing-keys* for old in before append (list key (- (packing-counter r key) old))))

(defun packing-expect (label actual expected)
  (if (equalp actual expected)
      (format t "PASS ~A ~S~%" label actual)
      (error "~A got ~S expected ~S" label actual expected)))

(defun packing-put (r column row &key (glyph "X") (mask 255) (fg #xffffff) (bg 0) (span 1) (line-size :single))
  (lispterm.gpu:renderer-write-cell
   r column row :glyph-key (list glyph nil span)
   :mask (make-array 1 :element-type '(unsigned-byte 8) :initial-element mask)
   :foreground fg :background bg :span span :line-size line-size))

(defun packing-signals-error-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun packing-call-with-failing (name thunk)
  "Run THUNK while the driver function NAME signals instead of reaching GL."
  (let ((saved (symbol-function name)))
    (unwind-protect
         (progn (setf (symbol-function name)
                      (lambda (&rest arguments) (declare (ignore arguments))
                        (error "injected ~A failure" name)))
                (funcall thunk))
      (setf (symbol-function name) saved))))

(defun packing-run (context rows oraclep)
  "Return the bytes consed by the one-cell draw."
  (let* ((columns 200) (one-cell-consed nil)
         (r (lispterm.gpu:make-renderer :context context :width columns :height rows
                                        :cell-width 1 :cell-height 1))
         (twin (lispterm.gpu:make-renderer :context context :width columns :height rows
                                           :cell-width 1 :cell-height 1)))
    (unwind-protect
         (labels ((both (function) (funcall function r) (funcall function twin))
                  (label (text) (format nil "rows=~D oracle=~A ~A" rows oraclep text))
                  (paint ()
                    (both (lambda (target)
                            (dotimes (y rows) (dotimes (x columns) (packing-put target x y :fg #x808080))))))
                  (frame (name &rest expected)
                    (let* ((before (packing-snapshot r))
                           (start (sb-ext:get-bytes-consed))
                           (consed (progn (lispterm.gpu:renderer-draw r)
                                          (- (sb-ext:get-bytes-consed) start)))
                           (after (packing-deltas r before))
                           (staged (lispterm.gpu::renderer-pack-staged r)))
                      (loop for (key value) on expected by #'cddr
                            do (packing-expect (label (format nil "~A ~S" name key)) (getf after key) value))
                      (let ((lispterm.gpu::*renderer-force-full-pack* t))
                        (lispterm.gpu:renderer-draw twin))
                      (packing-expect (label (format nil "~A pixels equal forced-full twin" name))
                              (mismatch (lispterm.gpu:renderer-readback r)
                                        (lispterm.gpu:renderer-readback twin))
                              nil)
                      (packing-expect (label (format nil "~A staged equal acknowledged octets" name))
                              (mismatch staged (lispterm.gpu::renderer-octets r)) nil)
                      (assert (equalp staged (lispterm.gpu::renderer-pack-staged twin)))
                      (format t "INFO ~A staged bytes ~:[differ from~;equal~] forced-full twin; consed ~D; ~S~%"
                              (label name)
                              (equalp staged (lispterm.gpu::renderer-pack-staged twin)) consed after)
                      (when oraclep
                        (packing-expect (label (format nil "~A staged equal fresh full pack" name))
                                (mismatch staged (values (lispterm.gpu::renderer-pack-instances r))) nil))
                      consed)))
           (paint)
           (frame "cold" :full-packs 1 :incremental-packs 0 :position-sorts 1
                         :records-encoded (* columns rows) :atlas-resolutions (* 2 columns rows))
           (both (lambda (target) (packing-put target 5 3 :fg #x123456)))
           (setf one-cell-consed
                 (frame "one-cell" :full-packs 0 :incremental-packs 1 :records-encoded 1
                                   :atlas-resolutions 1 :records-compared 1 :position-sorts 0
                                   :instance-upload-calls 1 :instance-bytes 32 :draw-calls 1
                                   :glyph-uploads 0))
           (both (lambda (target) (setf (lispterm.gpu::renderer-dirty target) t)))
           (frame "forced-redraw" :full-packs 0 :incremental-packs 1 :records-encoded 0
                                  :records-compared 0 :instance-upload-calls 0 :position-sorts 0)
           (both (lambda (target) (lispterm.gpu:renderer-write-remainder target 0 0 3 2 #xabcdef)))
           (frame "remainder-only" :full-packs 0 :incremental-packs 1 :records-encoded 0
                                   :instance-upload-calls 0 :position-sorts 0)
           ;; Repeated and out-of-order writes: two adjacent records and a distant one.
           (both (lambda (target)
                   (packing-put target 10 2 :fg #x111111) (packing-put target 9 2 :fg #x222222)
                   (packing-put target 150 (1- rows) :fg #x333333) (packing-put target 10 2 :fg #x444444)))
           (frame "three-cells" :full-packs 0 :incremental-packs 1 :records-encoded 3
                                :records-compared 3 :instance-upload-calls 2 :instance-bytes 96
                                :position-sorts 0)
           ;; Atlas capacity is 1 after the cold frame: a new glyph needs growth.
           (both (lambda (target) (packing-put target 20 4 :glyph "Y" :mask 0 :bg #x00ff00)))
           (frame "new-glyph-growth-fallback" :full-packs 1 :incremental-packs 0 :position-sorts 1)
           (both (lambda (target) (packing-put target 21 4 :glyph "Z" :mask 0 :bg #x0000ff)))
           (frame "second-growth-fallback" :full-packs 1 :incremental-packs 0)
           (packing-expect (label "atlas capacity after two growths")
                   (lispterm.gpu:atlas-capacity (lispterm.gpu::renderer-atlas r)) 4)
           (both (lambda (target) (packing-put target 22 4 :glyph "W" :mask 0 :bg #xff0000)))
           (frame "free-slot-fill" :full-packs 0 :incremental-packs 1 :records-encoded 1
                                   :atlas-resolutions 1 :glyph-uploads 1 :instance-upload-calls 1
                                   :instance-bytes 32)
           (both (lambda (target) (packing-put target 23 4 :glyph "Y" :mask 0 :bg #xffff00)))
           (frame "resident-hit" :full-packs 0 :incremental-packs 1 :records-encoded 1 :glyph-uploads 0)
           ;; Layout changes. A double-width cell swallows two single cells.
           (both (lambda (target) (packing-put target 2 7 :line-size :double-width :fg #xff00ff)))
           (frame "double-width-layout-change" :full-packs 1 :incremental-packs 0)
           (both (lambda (target) (packing-put target 2 7 :line-size :double-width :fg #x00ffff)))
           (frame "double-width-in-place" :full-packs 0 :incremental-packs 1 :records-encoded 1)
           ;; A double-width span-2 cell at column 0, split by a single cell at
           ;; column 1. Its left fragment has the key of a single cell at column 0.
           (both (lambda (target)
                   (packing-put target 0 11 :glyph "D" :span 2 :line-size :double-width :fg #xabcdef)))
           (frame "wide-at-column-0" :full-packs 1 :incremental-packs 0)
           (both (lambda (target) (packing-put target 1 11 :fg #xfedcba)))
           (frame "fragmenting-write" :full-packs 1 :incremental-packs 0)
           (packing-expect (label "clipped runs retained")
                   (and (lispterm.gpu::renderer-pack-runs r) t) t)
           (both (lambda (target) (packing-put target 0 11 :fg #x135790)))
           (frame "column-0-key-collision" :full-packs 1 :incremental-packs 0)
           (both (lambda (target) (packing-put target 0 11 :fg #x975310)))
           (let ((draws (packing-counter r :draw-calls)))
             (frame "in-place-with-clipped-runs" :full-packs 0 :incremental-packs 1 :records-encoded 1)
             (packing-expect (label "scissor runs drawn from the cache") (> (- (packing-counter r :draw-calls) draws) 1) t))
           ;; Failed instance upload, then retry.
           (both (lambda (target) (packing-put target 7 7 :fg #x0a0b0c)))
           (packing-expect (label "injected instance upload failure signals")
                   (packing-call-with-failing 'lispterm.gpu::gl-buffer-sub-data
                                      (lambda () (packing-signals-error-p (lambda () (lispterm.gpu:renderer-draw r)))))
                   t)
           (packing-expect (label "pack valid, record still dirty after failed instance upload")
                   (list (lispterm.gpu::renderer-pack-valid r)
                         (hash-table-count (lispterm.gpu::renderer-pack-dirty r))
                         (lispterm.gpu::renderer-dirty r))
                   '(t 1 t))
           (frame "retry-after-instance-failure" :full-packs 0 :incremental-packs 1 :records-encoded 0
                                                 :instance-upload-calls 1 :instance-bytes 32)
           ;; Failed glyph upload on the incremental path, then retry.
           (both (lambda (target) (packing-put target 8 7 :glyph "Q" :mask 0 :bg #x112233)))
           (packing-expect (label "injected glyph upload failure signals")
                   (packing-call-with-failing 'lispterm.gpu:gl-tex-sub-image-2d
                                      (lambda () (packing-signals-error-p (lambda () (lispterm.gpu:renderer-draw r)))))
                   t)
           (packing-expect (label "pack valid, glyph pending after failed glyph upload")
                   (list (lispterm.gpu::renderer-pack-valid r)
                         (length (lispterm.gpu:atlas-pending-uploads (lispterm.gpu::renderer-atlas r))))
                   '(t 1))
           (frame "retry-after-glyph-failure" :full-packs 0 :incremental-packs 1 :records-encoded 0
                                              :glyph-uploads 1 :instance-upload-calls 1)
           ;; A changed mask for a resident key signals from both packs.
           (packing-put r 8 8 :mask 0)
           (packing-expect (label "changed mask signals on the incremental pack")
                   (packing-signals-error-p (lambda () (lispterm.gpu:renderer-draw r))) t)
           (packing-expect (label "pack dropped after the error") (lispterm.gpu::renderer-pack-valid r) nil)
           (packing-expect (label "changed mask signals on the full pack")
                   (packing-signals-error-p (lambda () (lispterm.gpu:renderer-draw r))) t)
           (both (lambda (target) (packing-put target 8 8 :fg #x5a5a5a)))
           (frame "recovery-after-changed-mask" :full-packs 1 :incremental-packs 0)
           ;; A rejected resize keeps the retained pack; a successful one resets it.
           (packing-expect (label "rejected resize signals")
                   (packing-signals-error-p (lambda () (lispterm.gpu:renderer-resize r 0 rows 1 1))) t)
           (both (lambda (target) (packing-put target 30 5 :fg #x777777)))
           (frame "after-rejected-resize" :full-packs 0 :incremental-packs 1 :records-encoded 1)
           (both (lambda (target) (lispterm.gpu:renderer-resize target columns rows 1 1)))
           (packing-expect (label "successful resize resets the pack")
                   (list (lispterm.gpu::renderer-pack-valid r) (lispterm.gpu::renderer-pack-staged r)
                         (hash-table-count (lispterm.gpu::renderer-pack-index r)))
                   '(nil nil 0))
           (paint)
           (frame "after-resize" :full-packs 1 :incremental-packs 0)
           (both (lambda (target) (packing-put target 5 3 :fg #x123456)))
           (frame "one-cell-after-resize" :full-packs 0 :incremental-packs 1 :records-encoded 1
                                          :instance-bytes 32))
      (lispterm.gpu:renderer-close twin)
      (lispterm.gpu:renderer-close r))
    one-cell-consed))

(defun check-packing-work (context &optional (row-counts '(60 120)))
  "Count existing production operations and actual GL calls, independently of new metrics."
  (dolist (rows row-counts)
    (let ((r (lispterm.gpu:make-renderer :context context :width 200 :height rows
                                       :cell-width 1 :cell-height 1)))
      (unwind-protect
           (progn
             (dotimes (y rows) (dotimes (x 200) (packing-put r x y)))
             (lispterm.gpu:renderer-draw r)
             (packing-put r 5 3 :fg #x123456)
             (let* ((names '(lispterm.gpu::renderer-pack-instances
                             lispterm.gpu::renderer-positions
                             lispterm.gpu:atlas-ensure-glyph
                             lispterm.gpu:instance-buffer-write
                             lispterm.gpu:gl-buffer-sub-data
                             lispterm.gpu:gl-draw-arrays-instanced))
                    (saved (mapcar (lambda (n) (cons n (symbol-function n))) names))
                    (counts (make-hash-table)) (bytes 0))
               (unwind-protect
                    (progn
                      (dolist (entry saved)
                        (let ((name (car entry)) (real (cdr entry)))
                          (setf (symbol-function name)
                                (lambda (&rest args)
                                  (incf (gethash name counts 0))
                                  (when (eq name 'lispterm.gpu:gl-buffer-sub-data)
                                    (incf bytes (third args)))
                                  (apply real args)))))
                      (lispterm.gpu:renderer-draw r))
                 (dolist (entry saved) (setf (symbol-function (car entry)) (cdr entry))))
               (format t "PACKING-ACTUAL-WORK rows=~D calls=~S bytes=~D~%"
                       rows (loop for n in names collect (list n (gethash n counts 0))) bytes)
               ;; These checks also run against the old production definitions.
               (dolist (name (subseq names 0 4)) (assert (zerop (gethash name counts 0))))
               (assert (= 1 (gethash 'lispterm.gpu:gl-buffer-sub-data counts 0)))
               (assert (= 32 bytes))
               (assert (= 1 (gethash 'lispterm.gpu:gl-draw-arrays-instanced counts 0)))))
        (lispterm.gpu:renderer-close r)))))
(defun check-packing-capacity-recovery (context)
  (let ((r (lispterm.gpu:make-renderer :context context :width 8 :height 4 :cell-width 1 :cell-height 1))
        (twin (lispterm.gpu:make-renderer :context context :width 8 :height 4 :cell-width 1 :cell-height 1)))
    (unwind-protect
         (labels ((both (f) (funcall f r) (funcall f twin))
                  (frame ()
                    (lispterm.gpu:renderer-draw r)
                    (let ((lispterm.gpu::*renderer-force-full-pack* t)) (lispterm.gpu:renderer-draw twin))
                    (assert (equalp (lispterm.gpu:renderer-readback r) (lispterm.gpu:renderer-readback twin)))
                    (assert (equalp (lispterm.gpu::renderer-pack-staged r) (lispterm.gpu::renderer-octets r)))
                    (assert (equalp (lispterm.gpu::renderer-pack-staged r) (lispterm.gpu::renderer-pack-staged twin)))))
           (both (lambda (target)
                   (setf (slot-value (lispterm.gpu::renderer-atlas target) 'lispterm.gpu::maximum) 2)
                   (dotimes (y 4) (dotimes (x 8) (packing-put target x y :glyph "Y")))
                   (packing-put target 1 0 :glyph "X")))
           (frame)
           (let ((epoch (lispterm.gpu:atlas-epoch (lispterm.gpu::renderer-atlas r)))
                 (full (packing-counter r :full-packs)))
             (both (lambda (target) (packing-put target 1 0 :glyph "Z" :fg #x123456)))
             (frame)
             (assert (= 1 (- (packing-counter r :full-packs) full)))
             (assert (< epoch (lispterm.gpu:atlas-epoch (lispterm.gpu::renderer-atlas r)))))
           (both (lambda (target) (packing-put target 2 0 :glyph "W")))
           (dolist (target (list r twin))
             (assert (handler-case (progn (lispterm.gpu:renderer-draw target) nil)
                       (lispterm.gpu:atlas-capacity-error () t)))
             (assert (not (lispterm.gpu::renderer-pack-valid target)))
             (assert (lispterm.gpu::renderer-dirty target)))
           (both (lambda (target) (packing-put target 2 0 :glyph "Y")))
           (frame)
           (format t "PACKING-CAPACITY: eviction fallback, capacity refusal and recovery passed~%"))
      (lispterm.gpu:renderer-close twin) (lispterm.gpu:renderer-close r))))
(defun gpu-offscreen-check ()
  (let ((context (lispterm.gpu:open-context)) (renderer nil) (frames 0) (pixels 0))
    (unwind-protect
         (progn
           (lispterm.gpu:require-hardware-context context)
           (check-overlapping-cell-writes context)
           (check-packing-work context)
           (check-packing-capacity-recovery context)
           (dolist (rows '(60 120))
             (dolist (oraclep '(nil t))
               (let ((consed (packing-run context rows oraclep)))
                 ;; A fixed ceiling allows SBCL allocation-region noise, not grid-sized packing.
                 (assert (< consed 131072))
                 (format t "PACKING-ALLOCATION rows=~D oracle=~S draw-bytes=~D ceiling=131072~%" rows oraclep consed))))
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
