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

(defun check-renderer-construction-cleanup (context)
  "Fail construction and real GL deletion, then recover via the exposed owner."
  (let* ((allocate 'lispterm.gpu:gl-gen-buffers)
         (delete 'lispterm.gpu:gl-delete-vertex-arrays)
         (saved-allocate (symbol-function allocate))
         (saved-delete (symbol-function delete))
         (gpu-call (symbol-function 'lispterm.display::gpu-call))
         (partial nil) (receipt nil) (context-closes 0)
         (d (lispterm.display::make-display
             :font (lispterm.display::load-font nil 16)
             :owner sb-thread:*current-thread* :renderer-request :auto
             :width 16 :height 16)))
    (unwind-protect
         (progn
           (setf (symbol-function allocate)
                 (lambda (n p) (declare (ignore n)) (funcall saved-allocate -1 p))
                 (symbol-function delete)
                 (lambda (n p) (declare (ignore n)) (funcall saved-delete -1 p))
                 (symbol-function 'lispterm.display::gpu-call)
                 (lambda (name &rest args)
                   (cond ((string= name "OPEN-CONTEXT") context)
                         ((string= name "CLOSE-CONTEXT") (incf context-closes)
                          (error "Context must not close with retained renderer objects"))
                         (t (apply gpu-call name args)))))
           (setf receipt
                 (handler-case (progn (lispterm.display::initialize-display-renderer d) nil)
                   (lispterm.gpu:renderer-construction-cleanup-error (c) c)))
           (assert receipt)
           (setf partial (lispterm.gpu:construction-error-renderer receipt))
           (assert (eq partial (lispterm.display::display-renderer-state d)))
           (assert (eq context (lispterm.display::display-renderer-context d)))
           (assert (null (lispterm.display::display-renderer-fallback-reason d)))
           (assert (zerop context-closes))
           (assert (not (lispterm.gpu::renderer-closed partial)))
           (assert (plusp (lispterm.gpu::renderer-vao partial)))
           (assert (typep (lispterm.gpu:construction-error-cause receipt) 'lispterm.gpu:gpu-unavailable))
           (assert (search "#x501" (princ-to-string
                                   (lispterm.gpu:construction-error-cleanup-error receipt))))
           (setf (symbol-function delete) saved-delete)
           (lispterm.gpu:renderer-close partial)
           (assert (lispterm.gpu::renderer-closed partial))
           (assert (every #'zerop
                          (mapcar (lambda (f) (funcall f partial))
                                  '(lispterm.gpu::renderer-vao lispterm.gpu::renderer-vbo
                                    lispterm.gpu::renderer-fbo lispterm.gpu::renderer-program
                                    lispterm.gpu::renderer-texture))))
           (lispterm.gpu:gl-check 'construction-cleanup-retry)
           (format t "REAL-GL-CONSTRUCTION-CLEANUP retained owner/context, no auto fallback; explicit retry released handles~%"))
      (setf (symbol-function allocate) saved-allocate
            (symbol-function delete) saved-delete
            (symbol-function 'lispterm.display::gpu-call) gpu-call)
      (when partial (lispterm.gpu:renderer-close partial)))))

(defun check-display-resize-transaction (context)
  (let* ((cpu (parity-display 16 16)) (d (parity-display 16 16))
         (r (lispterm.gpu:make-renderer :context context :width 16 :height 16
                                      :cell-width 8 :cell-height 16)))
    (setf (lispterm.display::display-renderer-kind d) :gpu
          (lispterm.display::display-renderer-request d) :gpu
          (lispterm.display::display-renderer-state d) r)
    (unwind-protect
         (dolist (mode '(:bounds :stale))
           (let ((grid (lispterm.display::display-grid d))
                 (damage (lispterm.display::display-damage d))
                 (cache (lispterm.display::display-framebuffer-storage d))
                 (pending (lispterm.display::display-renderer-pending-p d))
                 (readback (lispterm.display::display-readback-pending-p d)))
             (when (eq mode :stale) (lispterm.gpu:gl-enable #xffffffff))
             (assert (handler-case
                         (progn (lispterm.display::resize-display
                                 d (if (eq mode :bounds) 200000 24) 32) nil)
                       (error () t)))
             (assert (= 16 (lispterm.display::display-width d)))
             (assert (= 16 (lispterm.display::display-height d)))
             (assert (= 2 (lispterm.display::display-columns d)))
             (assert (= 1 (lispterm.display::display-rows d)))
             (assert (eq grid (lispterm.display::display-grid d)))
             (assert (eq damage (lispterm.display::display-damage d)))
             (assert (eq cache (lispterm.display::display-framebuffer-storage d)))
             (assert (eql pending (lispterm.display::display-renderer-pending-p d)))
             (assert (eql readback (lispterm.display::display-readback-pending-p d)))
             (let ((cell (lispterm.display::make-cell :char #\R :fg #xabcdef :bg #x123456)))
               (lispterm.display::render-cpu-cell cpu 0 0 cell)
               (lispterm.display::render-cell d 0 0 cell)
               (assert (equalp (lispterm.display::display-framebuffer cpu)
                               (lispterm.display::display-framebuffer d))))
             (format t "DISPLAY-RESIZE-TRANSACTION ~A retained old geometry/cache/grid/damage; 256 exact old-size pixels after draw/readback~%" mode)))
      (lispterm.gpu:renderer-close r))))

(defun check-renderer-write-scaling (context)
  "Count actual spatial candidates, independently of wall time and GL uploads."
  (check-display-resize-transaction context)
  (dolist (rows '(60 120))
    (let* ((columns 200) (n (* columns rows))
           (r (lispterm.gpu:make-renderer :context context :width columns :height rows
                                        :cell-width 1 :cell-height 1))
           (mask (make-array 1 :element-type '(unsigned-byte 8) :initial-element 255)))
      (unwind-protect
           (flet ((paint ()
                    (dotimes (y rows)
                      (dotimes (x columns)
                        (lispterm.gpu:renderer-write-cell
                         r x y :glyph-key '("X" nil 1) :mask mask)))))
             (paint)
             (assert (zerop (gethash :cell-index-visits (lispterm.gpu::renderer-counters r) 0)))
             (paint)
             (assert (= n (gethash :cell-index-visits (lispterm.gpu::renderer-counters r))))
             (assert (= n (gethash :cell-intersection-tests (lispterm.gpu::renderer-counters r))))
             (dotimes (i 100)
               (lispterm.gpu:renderer-write-cell r (mod i columns) 0
                                                 :glyph-key '("X" nil 1) :mask mask))
             (assert (= (+ n 100) (gethash :cell-index-visits (lispterm.gpu::renderer-counters r))))
             (assert (= n (hash-table-count (lispterm.gpu::renderer-cells r))))
             (format t "SPATIAL-WRITE-SCALING cells=~D cold-visits=0 repaint-visits=~D warm100-visits=100~%" n n))
        (lispterm.gpu:renderer-close r)))))

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

;;;; Retry controls execute real GPU writes before the injected check failure.
(defparameter *retry-columns* 16)
(defparameter *retry-rows* 4)
(defparameter *retry-gray* #x808080)

(defun retry-expect (label actual expected)
  (if (equalp actual expected)
      (format t "PASS ~A ~S~%" label actual)
      (error "~A got ~S expected ~S" label actual expected)))

(defun retry-counter (r key) (gethash key (lispterm.gpu::renderer-counters r) 0))

(defun retry-put (r column row &key (glyph "X") (mask 255) (fg *retry-gray*) (bg 0) (line-size :single))
  (lispterm.gpu:renderer-write-cell
   r column row :glyph-key (list glyph nil 1)
   :mask (make-array 1 :element-type '(unsigned-byte 8) :initial-element mask)
   :foreground fg :background bg :line-size line-size))

(defun retry-paint (r)
  (dotimes (y *retry-rows*) (dotimes (x *retry-columns*) (retry-put r x y))))

(defun retry-draw (r &key full)
  (let ((lispterm.gpu::*renderer-force-full-pack* full))
    (lispterm.gpu:renderer-draw r)))

(defun retry-call-with-failing-check (operation nth thunk)
  "Run THUNK. The NTH gl-check of OPERATION signals GPU-UNAVAILABLE after the GL
call it guards and the real check both ran."
  (let ((real (symbol-function 'lispterm.gpu::gl-check)) (seen 0))
    (unwind-protect
         (progn (setf (symbol-function 'lispterm.gpu::gl-check)
                      (lambda (checked)
                        (funcall real checked)
                        (when (and (eq checked operation) (= (incf seen) nth))
                          (lispterm.gpu::unavailable checked "injected retry-failure after the GL call"))
                        t))
                (funcall thunk))
      (setf (symbol-function 'lispterm.gpu::gl-check) real))))

(defun retry-call-with-failing-driver (name nth thunk)
  "Run THUNK. The NTH call of the driver function NAME signals without reaching GL."
  (let ((real (symbol-function name)) (seen 0))
    (unwind-protect
         (progn (setf (symbol-function name)
                      (lambda (&rest arguments)
                        (if (= (incf seen) nth)
                            (error "injected ~A retry-failure before the GL call" name)
                            (apply real arguments))))
                (funcall thunk))
      (setf (symbol-function name) real))))

(defun retry-failure (thunk)
  "Return the type of the error THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil) (error (condition) (type-of condition))))

(defun retry-unknown-records (r)
  "Count of records whose GPU copy the renderer holds unknown, or :ABSENT before the repair."
  (let ((accessor (find-symbol "RENDERER-OCTETS-UNKNOWN" '#:lispterm.gpu)))
    (if (and accessor (fboundp accessor))
        (count 1 (funcall accessor r))
        :absent)))

(defun retry-pixel (pixels column row) (aref pixels (+ column (* *retry-columns* row))))

(defun retry-scenario (context name play &key retry-full retry-bytes retry-compared gray-cells)
  "PLAY receives a renderer and a function running one retry-draw thunk as the failing
frame, and leaves the retry pending. The retry is one retry-draw, forced full when
RETRY-FULL."
  (let ((r (lispterm.gpu:make-renderer :context context :width *retry-columns* :height *retry-rows*
                                       :cell-width 1 :cell-height 1))
        (twin (lispterm.gpu:make-renderer :context context :width *retry-columns* :height *retry-rows*
                                          :cell-width 1 :cell-height 1)))
    (unwind-protect
         (flet ((label (text) (format nil "~A: ~A" name text)))
           (funcall play twin (lambda (inject thunk) (declare (ignore inject)) (funcall thunk)))
           (funcall play r
                    (lambda (inject thunk)
                      (retry-expect (label "failing frame signals")
                              (and (retry-failure (lambda () (funcall inject thunk))) t) t)
                      (retry-expect (label "failing frame stays dirty for the caller's retry")
                              (lispterm.gpu::renderer-dirty r) t)))
           (let* ((bytes (retry-counter r :instance-bytes)) (compared (retry-counter r :records-compared))
                  (pixels (progn (retry-draw r :full retry-full) (lispterm.gpu:renderer-readback r)))
                  (wanted (progn (retry-draw twin :full retry-full) (lispterm.gpu:renderer-readback twin))))
             (retry-expect (label "retry pixels equal the never-failing twin") (mismatch pixels wanted) nil)
             (loop for (column row) in gray-cells
                   do (retry-expect (label (format nil "cell ~D,~D shows the reverted gray" column row))
                              (retry-pixel pixels column row) (retry-pixel pixels 0 (1- *retry-rows*))))
             (when retry-bytes
               (retry-expect (label "retry instance bytes") (- (retry-counter r :instance-bytes) bytes) retry-bytes))
             (when retry-compared
               (retry-expect (label "retry records compared")
                       (- (retry-counter r :records-compared) compared) retry-compared))
             (retry-expect (label "acknowledged octets equal the staged records")
                     (mismatch (lispterm.gpu::renderer-octets r) (lispterm.gpu::renderer-pack-staged r)) nil)
             (retry-expect (label "staged records equal the twin's")
                     (mismatch (lispterm.gpu::renderer-pack-staged r) (lispterm.gpu::renderer-pack-staged twin))
                     nil)
             (retry-expect (label "no unknown records, no dirty records, pack retained")
                     (list (retry-unknown-records r) (hash-table-count (lispterm.gpu::renderer-pack-dirty r))
                           (and (lispterm.gpu::renderer-pack-valid r) t))
                     (list (if (eq (retry-unknown-records r) :absent) :absent 0) 0 t))))
      (lispterm.gpu:renderer-close twin)
      (lispterm.gpu:renderer-close r))))

(defun retry-second-range-check (thunk) (retry-call-with-failing-check 'lispterm.gpu::instance-upload 2 thunk))

(defun retry-run (context)
  ;; Full pack. Two nonadjacent ranges; the first is uploaded and acknowledged,
  ;; the second fails. The first cell goes back to its old value before the retry.
  (retry-scenario context "full, first range reverted"
            (lambda (r failing)
              (retry-paint r) (retry-draw r)
              (retry-put r 1 0 :fg #xff0000) (retry-put r 9 2 :fg #x00ff00)
              (funcall failing #'retry-second-range-check (lambda () (retry-draw r :full t)))
              (retry-put r 1 0))
            :retry-full t :retry-bytes 64 :gray-cells '((1 0)))
  (retry-scenario context "full, first range reverted, second fails before its GL call"
            (lambda (r failing)
              (retry-paint r) (retry-draw r)
              (retry-put r 1 0 :fg #xff0000) (retry-put r 9 2 :fg #x00ff00)
              (funcall failing
                       (lambda (thunk) (retry-call-with-failing-driver 'lispterm.gpu::gl-buffer-sub-data 2 thunk))
                       (lambda () (retry-draw r :full t)))
              (retry-put r 1 0))
            :retry-full t :retry-bytes 64 :gray-cells '((1 0)))
  ;; The failing range itself is reverted: the buffer holds its new bytes.
  (retry-scenario context "full, failed range reverted"
            (lambda (r failing)
              (retry-paint r) (retry-draw r)
              (retry-put r 1 0 :fg #xff0000) (retry-put r 9 2 :fg #x00ff00)
              (funcall failing #'retry-second-range-check (lambda () (retry-draw r :full t)))
              (retry-put r 9 2))
            :retry-full t :retry-bytes 32 :gray-cells '((9 2)))
  ;; No test control: a new glyph needing atlas growth makes the incremental pack
  ;; fall back to the full pack, whose second range fails.
  (retry-scenario context "growth fallback, first range reverted"
            (lambda (r failing)
              (retry-paint r) (retry-draw r)
              (retry-put r 1 0 :fg #xff0000) (retry-put r 9 2 :glyph "Y" :mask 0 :bg #x00ff00)
              (let ((full (retry-counter r :full-packs)))
                (funcall failing #'retry-second-range-check (lambda () (retry-draw r)))
                (retry-expect "growth fallback: the failing frame ran the full pack"
                        (- (retry-counter r :full-packs) full) 1))
              (retry-put r 1 0))
            :retry-bytes 64 :gray-cells '((1 0)))
  ;; Allocation. 63 records, then 64 whose allocation fails, then the 63 old ones.
  (flet ((allocation (name inject)
           (retry-scenario context name
                     (lambda (r failing)
                       (retry-paint r) (retry-put r 2 0 :fg #x0000ff :line-size :double-width) (retry-draw r)
                       (retry-put r 4 0 :fg #xff0000)
                       (funcall failing inject (lambda () (retry-draw r)))
                       (retry-put r 2 0 :fg #x0000ff :line-size :double-width))
                     :retry-bytes (* 63 32) :retry-compared 0)))
    (allocation "allocation fails after its GL call, length reverts"
                (lambda (thunk) (retry-call-with-failing-check 'lispterm.gpu::instance-allocation 1 thunk)))
    (allocation "allocation fails before its GL call, length reverts"
                (lambda (thunk) (retry-call-with-failing-driver 'lispterm.gpu::gl-buffer-data 1 thunk))))
  ;; Incremental pack, same shapes.
  (retry-scenario context "incremental, both ranges reverted"
            (lambda (r failing)
              (retry-paint r) (retry-draw r)
              (retry-put r 1 0 :fg #xff0000) (retry-put r 9 2 :fg #x00ff00)
              (let ((incremental (retry-counter r :incremental-packs)))
                (funcall failing #'retry-second-range-check (lambda () (retry-draw r)))
                (retry-expect "incremental: the failing frame ran the incremental pack"
                        (- (retry-counter r :incremental-packs) incremental) 1))
              (retry-put r 1 0) (retry-put r 9 2))
            :retry-bytes 64 :gray-cells '((1 0) (9 2)))
  ;; A range the incremental upload left unknown is next seen by the full pack.
  (retry-scenario context "incremental failure, failed range reverted, full pack retries"
            (lambda (r failing)
              (retry-paint r) (retry-draw r)
              (retry-put r 1 0 :fg #xff0000) (retry-put r 9 2 :fg #x00ff00)
              (funcall failing #'retry-second-range-check (lambda () (retry-draw r)))
              (retry-put r 9 2))
            :retry-full t :retry-bytes 32 :gray-cells '((9 2))))

(defclass publication-renderer (lispterm.gpu::gpu-renderer) ())
(defun make-publication-renderer (&rest arguments)
  (change-class (apply #'lispterm.gpu:make-renderer arguments) 'publication-renderer))

(defparameter *publication-columns* 16)
(defparameter *publication-rows* 4)
(defparameter *publication-gray* #x808080)

(defvar *publication-tracing* nil "True while a publication-draw is observed.")
(defvar *publication-trace* nil "Events of the observed publication-draw, newest first.")
(defvar *publication-exit* nil "NIL, or (SLOT KIND): leave before the next non-NIL write of SLOT by KIND.")

(define-condition publication-injected-exit (error) ((slot :initarg :slot))
  (:report (lambda (condition stream)
             (format stream "injected exit before the ~A write" (slot-value condition 'slot)))))

(defun publication-expect (label actual expected)
  (if (equalp actual expected)
      (format t "PASS ~A ~S~%" label actual)
      (error "~A got ~S expected ~S" label actual expected)))

(defun publication-info (label value) (format t "INFO ~A ~S~%" label value))

(defun publication-call-observing-write (slot value write)
  "Run the slot write WRITE and record it, or leave before it when it is the armed one."
  (when (and *publication-tracing* value (eq slot (first *publication-exit*)))
    (let ((kind (second *publication-exit*)))
      (setf *publication-exit* nil)
      (push (list :exit-before-write slot kind) *publication-trace*)
      (ecase kind
        (:error (error 'publication-injected-exit :slot slot))
        (:throw (throw 'publication-injected-exit (list :throw slot))))))
  (multiple-value-prog1 (funcall write)
    (when *publication-tracing* (push (list :wrote slot (if value :value nil)) *publication-trace*))))

(defmethod (setf lispterm.gpu::renderer-octets) :around (value (r publication-renderer))
  (publication-call-observing-write :octets value #'call-next-method))

(defmethod (setf lispterm.gpu::renderer-octets-unknown) :around (value (r publication-renderer))
  (publication-call-observing-write :unknown value #'call-next-method))

(defun publication-call-observed (thunk)
  "Run THUNK recording real instance glBufferData calls, passed allocation checks
and publication-snapshot slot writes. Return how THUNK ended, the events in order, and the
report of the error that ended it."
  (let ((buffer-data (symbol-function 'lispterm.gpu::gl-buffer-data))
        (check (symbol-function 'lispterm.gpu::gl-check))
        (*publication-tracing* t) (*publication-trace* nil) (report nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'lispterm.gpu::gl-buffer-data)
                 (lambda (&rest arguments)
                   (multiple-value-prog1 (apply buffer-data arguments)
                     (when (eql (first arguments) #x8892)
                       (push (list :gl-buffer-data (second arguments)) *publication-trace*))))
                 (symbol-function 'lispterm.gpu::gl-check)
                 (lambda (&rest arguments)
                   (multiple-value-prog1 (apply check arguments)
                     (when (eq (first arguments) 'lispterm.gpu::instance-allocation)
                       (push (list :check-passed :instance-allocation) *publication-trace*)))))
           (let ((outcome (catch 'publication-injected-exit
                            (handler-case (progn (funcall thunk) :completed)
                              (error (condition)
                                (setf report (princ-to-string condition))
                                (list :error (type-of condition)))))))
             (values outcome (reverse *publication-trace*) report)))
      (setf (symbol-function 'lispterm.gpu::gl-buffer-data) buffer-data
            (symbol-function 'lispterm.gpu::gl-check) check))))

(defun publication-exit-after-allocation-p (trace)
  "True when TRACE shows glBufferData, then its passed check, then the injected exit."
  (let ((data (position :gl-buffer-data trace :key #'first))
        (check (position :check-passed trace :key #'first))
        (exit (position :exit-before-write trace :key #'first)))
    (and data check exit (< data check exit))))

(defun publication-counter (r key) (gethash key (lispterm.gpu::renderer-counters r) 0))

(defun publication-put (r column row &key (glyph "X") (mask 255) (fg *publication-gray*) (bg 0) (line-size :single))
  (lispterm.gpu:renderer-write-cell
   r column row :glyph-key (list glyph nil 1)
   :mask (make-array 1 :element-type '(unsigned-byte 8) :initial-element mask)
   :foreground fg :background bg :line-size line-size))

(defun publication-paint (r)
  (dotimes (y *publication-rows*) (dotimes (x *publication-columns*) (publication-put r x y))))

(defun publication-draw (r) (lispterm.gpu:renderer-draw r))

(defun publication-pixel (pixels column row) (aref pixels (+ column (* *publication-columns* row))))

(defun publication-snapshot (r)
  "The renderer's publication-snapshot slots as (OCTET-COUNT-OR-NIL BIT-COUNT-OR-NIL)."
  (let ((octets (lispterm.gpu::renderer-octets r)) (unknown (lispterm.gpu::renderer-octets-unknown r)))
    (list (and octets (length octets)) (and unknown (length unknown)))))

(defun publication-snapshot-usable-p (r)
  "True when the next publication-draw can use the snapshot: none, or one bit per record."
  (destructuring-bind (octets bits) (publication-snapshot r)
    (and (or (null octets) (and bits (= bits (floor octets 32)))) t)))

(defun publication-scenario (context name exit play &key marked-cells)
  "PLAY receives a renderer and a function running one publication-draw thunk as the frame
the exit EXIT leaves, and returns with the retry pending. The retry is drawn at
most twice. MARKED-CELLS must differ from the gray of cell 0 in the last row."
  (let ((r (make-publication-renderer :context context :width *publication-columns* :height *publication-rows*
                                       :cell-width 1 :cell-height 1))
        (twin (make-publication-renderer :context context :width *publication-columns* :height *publication-rows*
                                          :cell-width 1 :cell-height 1)))
    (unwind-protect
         (flet ((label (text) (format nil "~A: ~A" name text)))
           (funcall play twin #'funcall)
           (funcall play r
                    (lambda (thunk)
                      (multiple-value-bind (outcome trace)
                          (let ((*publication-exit* exit)) (publication-call-observed thunk))
                        (publication-info (label "failing frame events") trace)
                        (publication-expect (label "the injected exit left the frame") outcome
                                (ecase (second exit)
                                  (:error '(:error publication-injected-exit))
                                  (:throw (list :throw (first exit)))))
                        (publication-expect (label "the exit came after the real glBufferData and its passed check")
                                (publication-exit-after-allocation-p trace) t)
                        (publication-expect (label "the frame stays dirty for the caller's retry")
                                (lispterm.gpu::renderer-dirty r) t)
                        (publication-info (label "snapshot after the exit (octets bits)") (publication-snapshot r))
                        (publication-expect (label "snapshot after the exit is absent or has one bit per record")
                                (publication-snapshot-usable-p r) t))))
           (let* ((bytes (publication-counter r :instance-bytes)) (compared (publication-counter r :records-compared))
                  (retries (loop repeat 2
                                 collect (multiple-value-bind (outcome trace report)
                                             (publication-call-observed (lambda () (publication-draw r)))
                                           (publication-info (label "retry events") trace)
                                           (when report (publication-info (label "retry error") report))
                                           outcome)
                                   into outcomes
                                 until (eq (first (last outcomes)) :completed)
                                 finally (return outcomes)))
                  (pixels (handler-case (lispterm.gpu:renderer-readback r) (error () nil)))
                  (wanted (progn (publication-draw twin) (lispterm.gpu:renderer-readback twin)))
                  (staged (lispterm.gpu::renderer-pack-staged r))
                  (octets (lispterm.gpu::renderer-octets r))
                  (unknown (lispterm.gpu::renderer-octets-unknown r)))
             (publication-expect (label "the same-length retry completes at once") retries '(:completed))
             (publication-expect (label "retry pixels equal the never-failing twin")
                     (if pixels (mismatch pixels wanted) :no-readback) nil)
             (loop for (column row) in marked-cells
                   do (publication-expect (label (format nil "twin cell ~D,~D differs from gray" column row))
                              (/= (publication-pixel wanted column row) (publication-pixel wanted 0 (1- *publication-rows*))) t))
             (publication-expect (label "retry allocated again (instance bytes, records compared)")
                     (list (- (publication-counter r :instance-bytes) bytes) (- (publication-counter r :records-compared) compared))
                     (list (length (lispterm.gpu::renderer-pack-staged twin)) 0))
             (publication-expect (label "acknowledged octets equal the staged records")
                     (if octets (mismatch octets staged) :absent) nil)
             (publication-expect (label "staged records equal the twin's")
                     (mismatch staged (lispterm.gpu::renderer-pack-staged twin)) nil)
             (publication-expect (label "no unknown records, pack retained")
                     (list (if unknown (count 1 unknown) :absent)
                           (and (lispterm.gpu::renderer-pack-valid r) t))
                     '(0 t))))
      (lispterm.gpu:renderer-close twin)
      (lispterm.gpu:renderer-close r))))

(defun publication-run (context)
  (flet ((first-allocation (r failing)
           (publication-paint r) (publication-put r 1 0 :fg #xff0000)
           (funcall failing (lambda () (publication-draw r))))
         ;; 63 records drawn, then 64 whose publication is left, then one more
         ;; cell changes at the same 64 records before the retry.
         (regrown-allocation (r failing)
           (publication-paint r) (publication-put r 2 0 :fg #x0000ff :line-size :double-width) (publication-draw r)
           (publication-put r 4 0 :fg #xff0000)
           (funcall failing (lambda () (publication-draw r)))
           (publication-put r 9 2 :fg #x00ff00)))
    (publication-scenario context "first allocation, error before the unknown-bits write"
              '(:unknown :error) #'first-allocation :marked-cells '((1 0)))
    (publication-scenario context "first allocation, throw before the unknown-bits write"
              '(:unknown :throw) #'first-allocation :marked-cells '((1 0)))
    (publication-scenario context "63 -> 64 records, error before the unknown-bits write, cell changed before the retry"
              '(:unknown :error) #'regrown-allocation :marked-cells '((4 0) (9 2)))
    (publication-scenario context "first allocation, error before the octets write"
              '(:octets :error) #'first-allocation :marked-cells '((1 0)))
    (publication-scenario context "63 -> 64 records, throw before the octets write, cell changed before the retry"
              '(:octets :throw) #'regrown-allocation :marked-cells '((4 0) (9 2)))))

(defun gpu-evidence-check ()
  (let* ((context (lispterm.gpu:open-context)) (renderer nil)
         (cpu (parity-display 51 35))
         (grid (corpus-grid 6 2 0 :single))
         (changed-grid (corpus-grid 6 2 0 :single))
         (changed-cpu (parity-display 51 35)))
    (unwind-protect
         (progn
           (lispterm.gpu:require-hardware-context context)
           (retry-run context)
           (publication-run context)
           (check-renderer-delete-errors context)
           (check-renderer-partial-initialization context)
           (check-renderer-construction-cleanup context)
           (check-renderer-write-scaling context)
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
