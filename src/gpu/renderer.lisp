(in-package #:lispterm.gpu)

(defclass gpu-renderer ()
  ((context :initarg :context :reader renderer-context)
   (width :accessor renderer-width) (height :accessor renderer-height)
   (cw :accessor renderer-cw) (ch :accessor renderer-ch)
   (atlas :accessor renderer-atlas) (atlas-columns :accessor renderer-atlas-columns)
   (atlas-capacity :initform 0 :accessor renderer-atlas-capacity)
   (cells :initform (make-hash-table :test 'equal) :reader renderer-cells)
   ;; Fixed cell-sized spatial buckets contain retained rectangle keys.
   (cell-index :initform (make-hash-table :test 'equal) :reader renderer-cell-index)
   (remainders :initform (make-hash-table :test 'equal) :reader renderer-remainders)
   (program :initform 0 :accessor renderer-program)
   (texture :initform 0 :accessor renderer-texture)
   (target :initform 0 :accessor renderer-target)
   (fbo :initform 0 :accessor renderer-fbo)
   (vao :initform 0 :accessor renderer-vao)
   (vbo :initform 0 :accessor renderer-vbo)
   (pending-textures :initform nil :accessor renderer-pending-textures)
   ;; OCTETS is the instance buffer content the GPU acknowledged, NIL while no
   ;; allocation is acknowledged. OCTETS-UNKNOWN has one bit per record, set
   ;; while an upload of that record was started and not confirmed: the buffer
   ;; may then hold either version, so the record is uploaded whatever it equals.
   (octets :initform nil :accessor renderer-octets)
   (octets-unknown :initform nil :accessor renderer-octets-unknown)
   (dirty :initform t :accessor renderer-dirty)
   (closed :initform nil :accessor renderer-closed)
   ;; Incremental packing state, meaningful only while PACK-VALID. It is adopted
   ;; after a full pack and both its uploads succeed, and dropped by any write
   ;; that changes the cell layout. STAGED holds the wanted records; OCTETS keeps
   ;; meaning the bytes the GPU buffer acknowledged.
   (pack-valid :initform nil :accessor renderer-pack-valid)
   (pack-positions :initform nil :accessor renderer-pack-positions)
   (pack-index :initform (make-hash-table :test 'equal) :reader renderer-pack-index)
   (pack-staged :initform nil :accessor renderer-pack-staged)
   (pack-runs :initform nil :accessor renderer-pack-runs)
   (pack-epoch :initform nil :accessor renderer-pack-epoch)
   ;; Record index -> position of cells rewritten in place since the last pack.
   (pack-changed :initform (make-hash-table) :reader renderer-pack-changed)
   ;; Record indices whose staged bytes await an acknowledged upload.
   (pack-dirty :initform (make-hash-table) :reader renderer-pack-dirty)
   (metrics :initform (make-hash-table) :reader renderer-counters)))

(defun renderer-owner (r)
  (assert-context-owner (renderer-context r))
  (when (renderer-closed r) (error "Renderer is closed")))
(defun renderer-count (r name &optional (amount 1))
  (incf (gethash name (renderer-counters r) 0) amount))

(defvar *renderer-force-full-pack* nil
  "Test control: when true, RENDERER-DRAW takes the full pack for every dirty frame.")

(defun renderer-invalidate-pack (r)
  "Make the next dirty draw run the full pack."
  (setf (renderer-pack-valid r) nil)
  (clrhash (renderer-pack-changed r))
  (clrhash (renderer-pack-dirty r)))

(defun renderer-reset-pack (r)
  (renderer-invalidate-pack r)
  (clrhash (renderer-pack-index r))
  (setf (renderer-pack-positions r) nil (renderer-pack-staged r) nil
        (renderer-pack-runs r) nil (renderer-pack-epoch r) nil))

(defun renderer-name (generator)
  (cffi:with-foreign-object (p :unsigned-int)
    (gl-check 'before-allocate-renderer-name)
    (setf (cffi:mem-ref p :unsigned-int) 0)
    (funcall generator 1 p)
    (gl-check 'allocate-renderer-name)
    (cffi:mem-ref p :unsigned-int)))
(defun renderer-delete-name (deleter name)
  (unless (zerop name)
    ;; A stale error is not evidence about the deletion. Refuse before touching
    ;; ownership, then check the result while the caller still owns the name.
    (gl-check 'before-renderer-delete)
    (cffi:with-foreign-object (p :unsigned-int)
      (setf (cffi:mem-ref p :unsigned-int) name)
      (funcall deleter 1 p)
      (gl-check 'renderer-delete))))
(defun renderer-color (rgb)
  (gl-clear-color (/ (ldb (byte 8 16) rgb) 255.0)
                  (/ (ldb (byte 8 8) rgb) 255.0)
                  (/ (ldb (byte 8 0) rgb) 255.0) 1.0))

(defgeneric renderer-close (renderer))
(defmethod renderer-close :around ((r gpu-renderer))
  ;; Keep partial-construction ownership reachable while handling a cleanup error.
  (loop
    (restart-case (return (call-next-method))
      (retry-renderer-close () :report "Retry releasing the retained renderer objects."))))
(defmethod renderer-close ((r gpu-renderer))
  (sb-sys:without-interrupts
    (unless (renderer-closed r)
      (renderer-owner r)
      ;; Check and release each owned name atomically with respect to Lisp
      ;; interruption. Failed deletions retain their handle for a cleanup retry.
      (macrolet ((release (accessor function)
                   `(progn (renderer-delete-name #',function (,accessor r))
                           (setf (,accessor r) 0))))
        (release renderer-vbo gl-delete-buffers)
        (release renderer-vao gl-delete-vertex-arrays)
        (release renderer-fbo gl-delete-framebuffers)
        (release renderer-target gl-delete-textures)
        (release renderer-texture gl-delete-textures))
      (dolist (texture (copy-list (renderer-pending-textures r)))
        (renderer-delete-name #'gl-delete-textures texture)
        (setf (renderer-pending-textures r) (remove texture (renderer-pending-textures r))))
      (unless (zerop (renderer-program r))
        (gl-check 'before-renderer-delete-program)
        (gl-delete-program (renderer-program r))
        (gl-check 'renderer-delete-program)
        (setf (renderer-program r) 0))
      (setf (renderer-closed r) t)))
  t)

(define-condition renderer-construction-cleanup-error (error)
  ((renderer :initarg :renderer :reader construction-error-renderer)
   (cause :initarg :cause :reader construction-error-cause)
   (cleanup-error :initarg :cleanup-error :reader construction-error-cleanup-error))
  (:report (lambda (condition stream)
             (format stream "Renderer construction failed (~A); cleanup failed (~A). Retained renderer requires owner-thread close."
                     (construction-error-cause condition)
                     (construction-error-cleanup-error condition)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(renderer-construction-cleanup-error construction-error-renderer
            construction-error-cause construction-error-cleanup-error)))

(defgeneric renderer-resize (renderer width height cell-width cell-height))
(defmethod renderer-resize ((r gpu-renderer) width height cell-width cell-height)
  (renderer-owner r)
  (sb-sys:without-interrupts
    (gl-check 'before-renderer-resize)
    (let* ((limit (gl-integer #x0D33))
           (viewport (gl-integers #x0D3A 2)))
      (dolist (v (list width height cell-width cell-height))
        (model-positive-integer v :dimension (min limit (expt 2 24))))
      (unless (and (<= width (first viewport)) (<= height (second viewport))
                   (<= (* width height 4) #x7fffffff)
                   (<= (* (max 1 (floor width cell-width))
                          (max 1 (floor height cell-height))) 1048576)
                   (<= (* 4 (max width cell-width)) (expt 2 24))
                   (<= (* 4 (max height cell-height)) (expt 2 24)))
        (error "Renderer dimensions exceed device or exact shader arithmetic bounds"))
      ;; Prepare Lisp storage before committing the GL target replacement too.
      (let* ((columns (min 64 (floor limit cell-width)))
             (atlas (make-atlas :tile-width cell-width :tile-height cell-height :capacity 1
                                :max-capacity (min 65536 (* columns (floor limit cell-height)))))
             (texture (renderer-name #'gl-gen-textures)) (success nil))
        ;; Retain provisional ownership if rollback deletion itself fails.
        (push texture (renderer-pending-textures r))
        (unwind-protect
             (progn
               (gl-bind-texture #x0DE1 texture)
               (gl-tex-parameter-i #x0DE1 #x2801 #x2600)
               (gl-tex-parameter-i #x0DE1 #x2800 #x2600)
               (gl-tex-image-2d #x0DE1 0 #x8058 width height 0 #x1908 #x1401 (cffi:null-pointer))
               (gl-bind-framebuffer #x8D40 (renderer-fbo r))
               (gl-framebuffer-texture-2d #x8D40 #x8CE0 #x0DE1 texture 0)
               (gl-check 'renderer-resize)
               (unless (= (gl-check-framebuffer-status #x8D40) #x8CD5)
                 (unavailable 'renderer-resize "Incomplete renderer framebuffer"))
               (renderer-delete-name #'gl-delete-textures (renderer-target r))
               (setf (renderer-target r) texture success t
                     (renderer-pending-textures r) (remove texture (renderer-pending-textures r))))
          (unless success
            (gl-bind-framebuffer #x8D40 (renderer-fbo r))
            (gl-framebuffer-texture-2d #x8D40 #x8CE0 #x0DE1 (renderer-target r) 0)
            (gl-check 'renderer-resize-rollback)
            (renderer-delete-name #'gl-delete-textures texture)
            (setf (renderer-pending-textures r) (remove texture (renderer-pending-textures r)))))
        (setf (renderer-width r) width (renderer-height r) height
              (renderer-cw r) cell-width (renderer-ch r) cell-height
              (renderer-atlas-columns r) columns (renderer-atlas r) atlas
              (renderer-atlas-capacity r) 0
              (renderer-octets r) nil (renderer-octets-unknown r) nil (renderer-dirty r) t)
        (clrhash (renderer-cells r)) (clrhash (renderer-cell-index r))
        (clrhash (renderer-remainders r))
        (renderer-reset-pack r)
        (renderer-count r :generations))))
  r)

(defun make-renderer (&key context width height cell-width cell-height owner)
  "Expose partial ownership through OWNER before allocating any GL objects.
If cleanup fails, the signaled condition also retains the renderer for explicit retry."
  (assert-context-owner context)
  (sb-sys:without-interrupts
    (let ((r (make-instance 'gpu-renderer :context context)) (success nil) (cause nil))
      (when owner (funcall owner r))
      (unwind-protect
           (handler-bind ((error (lambda (condition) (setf cause condition))))
             (setf (renderer-program r)
                   (link-gl-program (emit-shader (shader-form :vertex))
                                    (emit-shader (shader-form :fragment)))
                   (renderer-fbo r) (renderer-name #'gl-gen-framebuffers)
                   (renderer-texture r) (renderer-name #'gl-gen-textures)
                   (renderer-vao r) (renderer-name #'gl-gen-vertex-arrays)
                   (renderer-vbo r) (renderer-name #'gl-gen-buffers))
             (renderer-resize r width height cell-width cell-height)
             (setf success t)
             r)
        (unless success
          (handler-case (renderer-close r)
            (error (cleanup)
              (error 'renderer-construction-cleanup-error :renderer r
                     :cause (or cause "nonlocal construction exit") :cleanup-error cleanup))))))))

(defun renderer-subtract-remainders (r left top right bottom)
  "Keep only old background-strip pixels outside a newly overwritten rectangle."
  (let ((remove nil) (pieces nil))
    (maphash (lambda (rectangle color)
               (destructuring-bind (x y width height) rectangle
                 (let ((ix (max x left)) (iy (max y top))
                       (ir (min (+ x width) right)) (ib (min (+ y height) bottom)))
                   (when (and (< ix ir) (< iy ib))
                     (push rectangle remove)
                     (flet ((piece (px py pw ph)
                              (when (and (plusp pw) (plusp ph))
                                (push (cons (list px py pw ph) color) pieces))))
                       (piece x y width (- iy y))
                       (piece x ib width (- (+ y height) ib))
                       (piece x iy (- ix x) (- ib iy))
                       (piece ir iy (- (+ x width) ir) (- ib iy)))))))
             (renderer-remainders r))
    (dolist (rectangle remove) (remhash rectangle (renderer-remainders r)))
    (dolist (piece pieces) (setf (gethash (car piece) (renderer-remainders r)) (cdr piece)))))

(defun renderer-rectangle-buckets (r rectangle function)
  "Visit fixed-size spatial tiles touched by a nonempty half-open rectangle."
  (destructuring-bind (left top right bottom) rectangle
    (when (and (< left right) (< top bottom))
      (loop for y from (floor top (renderer-ch r)) below (ceiling bottom (renderer-ch r))
            do (loop for x from (floor left (renderer-cw r)) below (ceiling right (renderer-cw r))
                     do (funcall function (list x y)))))))

(defun renderer-index-cell (r position cell &optional remove-p)
  (renderer-rectangle-buckets
   r (getf cell :rectangle)
   (lambda (tile)
     (let ((bucket (gethash tile (renderer-cell-index r))))
       (if remove-p
           (when bucket
             (remhash position bucket)
             (when (zerop (hash-table-count bucket))
               (remhash tile (renderer-cell-index r))))
           (progn
             (unless bucket
               (setf bucket (make-hash-table :test 'equal)
                     (gethash tile (renderer-cell-index r)) bucket))
             (setf (gethash position bucket) t)))))))

(defun renderer-subtract-cells (r left top right bottom)
  "Subtract coverage from nearby retained fragments without scanning the frame.
Return the removed position keys and the clipped (key . cell) pieces created."
  (let ((candidates (make-hash-table :test 'equal)) (remove nil) (pieces nil))
    (renderer-rectangle-buckets
     r (list left top right bottom)
     (lambda (tile)
       (let ((bucket (gethash tile (renderer-cell-index r))))
         (when bucket
           (maphash (lambda (position present)
                      (declare (ignore present))
                      (renderer-count r :cell-index-visits)
                      (setf (gethash position candidates) t)) bucket)))))
    (maphash
     (lambda (position present)
       (declare (ignore present))
       (renderer-count r :cell-intersection-tests)
       (let ((cell (gethash position (renderer-cells r))))
         (destructuring-bind (x y end-x end-y) (getf cell :rectangle)
           (let ((ix (max x left)) (iy (max y top))
                 (ir (min end-x right)) (ib (min end-y bottom)))
             (when (and (< ix ir) (< iy ib))
               (push position remove)
               (flet ((piece (px py pr pb)
                        (when (and (< px pr) (< py pb))
                          (let* ((rectangle (list px py pr pb)) (copy (copy-list cell)))
                            (setf (getf copy :rectangle) rectangle (getf copy :clipped) t)
                            (push (cons (append (subseq position 0 2) rectangle) copy) pieces)))))
                 (piece x y end-x iy) (piece x ib end-x end-y)
                 (piece x iy ix ib) (piece ir iy end-x ib)))))))
     candidates)
    (dolist (position remove)
      (renderer-index-cell r position (gethash position (renderer-cells r)) t)
      (remhash position (renderer-cells r)))
    (dolist (piece pieces)
      (setf (gethash (car piece) (renderer-cells r)) (cdr piece))
      (renderer-index-cell r (car piece) (cdr piece)))
    (values remove pieces)))

(defun renderer-positions (r)
  ;; Shared ordering for the packed records and the optional scissor draws.
  (renderer-count r :position-sorts)
  (sort (loop for p being the hash-keys of (renderer-cells r) collect p)
        (lambda (a b)
          (or (< (second a) (second b))
              (and (= (second a) (second b))
                   (or (< (first a) (first b))
                       (and (= (first a) (first b))
                          (loop for x in (cddr a) for y in (cddr b)
                                when (/= x y) return (< x y)))))))))

(defun renderer-note-write (r position preserved)
  "Track an in-place cell rewrite for the next incremental pack, or drop the pack."
  (when (renderer-pack-valid r)
    (let ((index (and preserved (gethash position (renderer-pack-index r)))))
      (if index
          (setf (gethash index (renderer-pack-changed r)) position)
          (renderer-invalidate-pack r)))))

(defgeneric renderer-write-cell (renderer column row &key glyph-key mask foreground background
                                                         bold underline hidden span line-size))
(defmethod renderer-write-cell ((r gpu-renderer) column row
                                &key glyph-key mask (foreground 0) (background 0)
                                  bold underline hidden (span 1) (line-size :single))
  (renderer-owner r)
  (let* ((scale (if (eq line-size :single) 1 2))
         (columns (max 1 (floor (renderer-width r) (renderer-cw r))))
         (rows (max 1 (floor (renderer-height r) (renderer-ch r))))
         (key (snapshot-glyph-key glyph-key))
         (buffer (make-instance-buffer :columns 1 :rows 1)))
    (instance-unsigned column (1- (max 1 (floor columns scale))) :column)
    (instance-unsigned row (1- rows) :row)
    ;; Reuse the record validator without accepting huge or unvalidated fields.
    (instance-buffer-write buffer 0 0 :foreground foreground :background background
                           :bold bold :underline underline :hidden hidden :span span :line-size line-size)
    (checked-mask (renderer-atlas r) mask)
    (unless (and (eql (second key) bold) (= (third key) span))
      (error "Glyph key bold/span disagree with instance"))
    ;; Subtract new cell coverage from retained background strips. Pixels outside
    ;; the logical grid keep their previous remainder color, exactly as the CPU.
    (renderer-subtract-remainders
     r (* column (renderer-cw r) scale) (* row (renderer-ch r))
     (min (renderer-width r) (* (renderer-cw r) scale (max 1 (floor columns scale)))
          (* (+ column span) (renderer-cw r) scale))
     (* (1+ row) (renderer-ch r)))
    (let* ((left (* column (renderer-cw r) scale))
           (top (* row (renderer-ch r)))
           (right (min (renderer-width r)
                       (* (renderer-cw r) scale (max 1 (floor columns scale)))
                       (* (+ column span) (renderer-cw r) scale)))
           (bottom (* (1+ row) (renderer-ch r)))
           (position (list column row left top right bottom))
           (old (gethash position (renderer-cells r)))
           (cell (list :key key :mask (copy-seq mask) :foreground foreground :background background
                       :bold bold :underline underline :hidden hidden :span span :line-size line-size
                       :rectangle (list left top right bottom)))
           (preserved nil))
      ;; The packed layout survives only a write replacing exactly one whole cell
      ;; at its own position. A clipped fragment can share that key, so the old
      ;; cell must be unclipped. Everything else, including a non-local exit from
      ;; the mutation, drops the retained pack.
      (unwind-protect
           (multiple-value-bind (removed pieces) (renderer-subtract-cells r left top right bottom)
             (setf (gethash position (renderer-cells r)) cell)
             (renderer-index-cell r position cell)
             (setf preserved (and old (not (getf old :clipped)) (null pieces)
                                  (equal removed (list position)))))
        (renderer-note-write r position preserved)))
    (setf (renderer-dirty r) t))
  r)

(defgeneric renderer-write-remainder (renderer x y width height background))
(defmethod renderer-write-remainder ((r gpu-renderer) x y width height background)
  (renderer-owner r)
  (dolist (v (list x y width height)) (instance-unsigned v (expt 2 24) :rectangle))
  (instance-unsigned background #xffffff :background)
  (unless (and (<= (+ x width) (renderer-width r)) (<= (+ y height) (renderer-height r)))
    (error "Remainder outside framebuffer"))
  (renderer-subtract-remainders r x y (+ x width) (+ y height))
  (setf (gethash (list x y width height) (renderer-remainders r)) background
        (renderer-dirty r) t)
  r)

(defun renderer-upload-atlas (r)
  (let* ((atlas (renderer-atlas r)) (capacity (atlas-capacity atlas))
         (columns (renderer-atlas-columns r)) (cw (renderer-cw r)) (ch (renderer-ch r)))
    (gl-active-texture #x84C0)
    (gl-bind-texture #x0DE1 (renderer-texture r))
    (gl-pixel-store-i #x0CF5 1)
    (unless (= capacity (renderer-atlas-capacity r))
      (gl-tex-parameter-i #x0DE1 #x2801 #x2600)
      (gl-tex-parameter-i #x0DE1 #x2800 #x2600)
      (gl-tex-parameter-i #x0DE1 #x813C 0)
      (gl-tex-parameter-i #x0DE1 #x813D 0)
      (gl-tex-image-2d #x0DE1 0 #x8229 (* columns cw) (* (ceiling capacity columns) ch)
                       0 #x1903 #x1401 (cffi:null-pointer))
      (gl-check 'atlas-storage)
      (setf (renderer-atlas-capacity r) capacity)
      (renderer-count r :atlas-allocations))
    (dolist (upload (atlas-pending-uploads atlas))
      (let ((mask (getf upload :mask)) (slot (getf upload :slot)) (epoch (getf upload :epoch)))
        (unless (atlas-reference-valid-p atlas slot epoch (getf upload :key))
          (error "Stale atlas upload"))
        (cffi:with-pointer-to-vector-data (p mask)
          (gl-tex-sub-image-2d #x0DE1 0 (* (mod slot columns) cw) (* (floor slot columns) ch)
                               cw ch #x1903 #x1401 p))
        (gl-check 'atlas-upload)
        (renderer-count r :glyph-uploads) (renderer-count r :glyph-bytes (length mask))
        (atlas-acknowledge-upload atlas slot epoch)))))

(defun renderer-pack-instances (r)
  "Full pack: declare, ensure and encode every cell. The fallback for each frame
the incremental pack cannot prove safe, and its oracle. Return the packed
records and their sorted positions."
  (let* ((positions (renderer-positions r))
         (atlas (renderer-atlas r))
         (columns (max 1 (floor (renderer-width r) (renderer-cw r))))
         (rows (max 1 (floor (renderer-height r) (renderer-ch r))))
         (buffer (make-instance-buffer :columns columns :rows rows :slot-limit (atlas-max-capacity atlas)))
         (packed (make-array (* 32 (length positions)) :element-type '(unsigned-byte 8))))
    (atlas-begin-frame atlas (mapcar (lambda (p) (getf (gethash p (renderer-cells r)) :key)) positions))
    ;; Complete every ensure before using an epoch or acknowledging any uploads.
    (dolist (p positions)
      (let ((cell (gethash p (renderer-cells r))))
        (atlas-ensure-glyph atlas (getf cell :key) (getf cell :mask))))
    (loop for p in positions for index from 0
          for cell = (gethash p (renderer-cells r))
          do (multiple-value-bind (slot epoch) (atlas-ensure-glyph atlas (getf cell :key) (getf cell :mask))
               (unless (atlas-reference-valid-p atlas slot epoch (getf cell :key)) (error "Stale instance"))
               (apply #'instance-buffer-write buffer (first p) (second p) :slot slot
                      (loop for k in '(:foreground :background :bold :underline :hidden :span :line-size)
                            append (list k (getf cell k))))
               (let ((offset (* 32 (+ (first p) (* columns (second p))))))
                 (replace packed (instance-buffer-octets buffer) :start1 (* index 32)
                          :start2 offset :end2 (+ offset 32)))))
    (renderer-count r :atlas-resolutions (* 2 (length positions)))
    (renderer-count r :records-encoded (length positions))
    (values packed positions)))

(defun renderer-draw-runs (r positions)
  "Split sorted POSITIONS into (first-record count scissor-rectangle) runs.
Only a clipped fragment has a rectangle, and a run of its own. Return NIL when
nothing is clipped: the frame is one unscissored instanced draw."
  (let ((cells (renderer-cells r)))
    (when (some (lambda (p) (getf (gethash p cells) :clipped)) positions)
      (loop with remaining = positions with index = 0
            while remaining
            collect (let* ((cell (gethash (first remaining) cells))
                           (clipped (getf cell :clipped))
                           (n (if clipped 1
                                  (loop for p in remaining
                                        until (getf (gethash p cells) :clipped)
                                        count t))))
                      (prog1 (list index n (and clipped (copy-list (getf cell :rectangle))))
                        (incf index n)
                        (setf remaining (nthcdr n remaining))))))))

(defun renderer-retain-pack (r packed positions)
  "Adopt a full pack once its atlas and instance uploads have both succeeded."
  (let ((index (renderer-pack-index r)) (staged (renderer-pack-staged r)))
    (clrhash index)
    (loop for p in positions for i from 0 do (setf (gethash p index) i))
    (clrhash (renderer-pack-changed r))
    (clrhash (renderer-pack-dirty r))
    (setf (renderer-pack-positions r) positions
          (renderer-pack-staged r) (if (and staged (= (length staged) (length packed)))
                                       (replace staged packed)
                                       (copy-seq packed))
          (renderer-pack-runs r) (renderer-draw-runs r positions)
          (renderer-pack-epoch r) (atlas-epoch (renderer-atlas r))
          (renderer-pack-valid r) t)))

(defun renderer-stage-record (r index position columns rows)
  "Resolve and re-encode one rewritten cell. Return NIL when its glyph needs the
full frame's eviction or growth."
  (let ((cell (gethash position (renderer-cells r))) (atlas (renderer-atlas r)))
    (when cell
      (renderer-count r :atlas-resolutions)
      (multiple-value-bind (slot epoch)
          (atlas-resolve-settled-glyph atlas (getf cell :key) (getf cell :mask))
        (when slot
          (unless (atlas-reference-valid-p atlas slot epoch (getf cell :key)) (error "Stale instance"))
          (renderer-count r :records-encoded)
          (when (apply #'instance-encode-record (renderer-pack-staged r) (* index 32)
                       columns rows (atlas-max-capacity atlas) (first position) (second position)
                       :slot slot
                       (loop for k in '(:foreground :background :bold :underline :hidden :span :line-size)
                             append (list k (getf cell k))))
            (setf (gethash index (renderer-pack-dirty r)) t))
          t)))))

(defun renderer-pack-incremental (r)
  "Re-encode only the cells rewritten in place since the retained pack.
Return true when the staged records describe the current cells. Return NIL with
the retained pack dropped when this draw must run the full pack: no valid pack,
a moved atlas epoch, or a glyph needing eviction or growth. Errors drop the
pack too and propagate."
  (let ((atlas (renderer-atlas r)) (staged (renderer-pack-staged r))
        (octets (renderer-octets r)) (changed (renderer-pack-changed r)) (current nil))
    (unwind-protect
         (when (and (renderer-pack-valid r) (not *renderer-force-full-pack*)
                    (eql (renderer-pack-epoch r) (atlas-epoch atlas))
                    staged octets (= (length staged) (length octets)))
           (let ((columns (max 1 (floor (renderer-width r) (renderer-cw r))))
                 (rows (max 1 (floor (renderer-height r) (renderer-ch r)))))
             ;; Ascending record order fills free atlas slots as the full pack would.
             (setf current
                   (loop for index in (sort (loop for index being the hash-keys of changed
                                                  collect index) #'<)
                         always (renderer-stage-record r index (gethash index changed) columns rows))))
           (when current
             (clrhash changed)
             (renderer-count r :incremental-packs)))
      (unless current (renderer-invalidate-pack r)))
    current))

(defun renderer-record-stale-p (r source offset)
  "True unless the GPU buffer is known to hold SOURCE's record at byte OFFSET."
  (or (= 1 (sbit (renderer-octets-unknown r) (floor offset 32)))
      (mismatch (renderer-octets r) source :start1 offset :end1 (+ offset 32)
                                           :start2 offset :end2 (+ offset 32))))

(defun renderer-upload-range (r source start end)
  "Upload bytes START to END of SOURCE into the bound instance buffer and
acknowledge them. The records are unknown from before the GL call until its
check passed, so every failure and non-local exit leaves them to be uploaded
again."
  (let ((unknown (renderer-octets-unknown r))
        (first (floor start 32)) (last (floor end 32)))
    (fill unknown 1 :start first :end last)
    (cffi:with-pointer-to-vector-data (p source)
      (gl-buffer-sub-data #x8892 start (- end start) (cffi:inc-pointer p start)))
    (gl-check 'instance-upload)
    (replace (renderer-octets r) source :start1 start :end1 end :start2 start)
    (fill unknown 0 :start first :end last)
    (renderer-count r :instance-upload-calls)
    (renderer-count r :instance-bytes (- end start))))

(defun renderer-upload-instances (r packed)
  "Make the GPU instance buffer hold PACKED: allocate on the first upload or a
length change, else upload the stale records as merged ranges, acknowledging
each range once its own check passed."
  (let ((old (renderer-octets r)))
    (gl-bind-buffer #x8892 (renderer-vbo r))
    (cond ((or (null old) (/= (length old) (length packed)))
           ;; Allocate the unknown bits while the old snapshot still stands.
           (let ((unknown (make-array (floor (length packed) 32)
                                      :element-type 'bit :initial-element 0)))
             ;; A failed allocation leaves the buffer's size and content unknown.
             ;; With no snapshot the retry allocates again, whatever its length.
             (setf (renderer-octets r) nil (renderer-octets-unknown r) nil)
             (if (plusp (length packed))
                 (cffi:with-pointer-to-vector-data (p packed)
                   (gl-buffer-data #x8892 (length packed) p #x88E8))
                 (gl-buffer-data #x8892 0 (cffi:null-pointer) #x88E8))
             (gl-check 'instance-allocation)
             ;; The octets are the snapshot. Publish them last, so an exit
             ;; between the two writes leaves no snapshot.
             (setf (renderer-octets-unknown r) unknown
                   (renderer-octets r) packed))
           (renderer-count r :instance-upload-calls)
           (renderer-count r :instance-bytes (length packed)))
          (t
           (renderer-count r :records-compared (floor (length packed) 32))
           (loop with start = nil for offset from 0 to (length packed) by 32
                 for stale = (and (< offset (length packed))
                                  (renderer-record-stale-p r packed offset))
                 do (cond (stale (unless start (setf start offset)))
                          (start (renderer-upload-range r packed start offset)
                                 (setf start nil))))))))

(defun renderer-upload-staged (r)
  "Upload the stale dirty staged records as merged ranges. A failure keeps the
dirty set, so the next draw retries every range the GPU buffer did not
acknowledge."
  (let ((staged (renderer-pack-staged r)) (dirty (renderer-pack-dirty r)) (ranges nil))
    (gl-bind-buffer #x8892 (renderer-vbo r))
    (dolist (index (sort (loop for index being the hash-keys of dirty collect index) #'<))
      (let ((offset (* index 32)))
        (renderer-count r :records-compared)
        (when (renderer-record-stale-p r staged offset)
          (if (and ranges (= (cdr (first ranges)) offset))
              (setf (cdr (first ranges)) (+ offset 32))
              (push (cons offset (+ offset 32)) ranges)))))
    (loop for (start . end) in (nreverse ranges)
          do (renderer-upload-range r staged start end))
    (clrhash dirty)))

(defgeneric renderer-draw (renderer))
(defmethod renderer-draw ((r gpu-renderer))
  (renderer-owner r)
  (when (renderer-dirty r)
    (let ((start (get-internal-real-time)))
      (if (renderer-pack-incremental r)
          (progn (renderer-upload-atlas r)
                 (gl-bind-vertex-array (renderer-vao r))
                 (renderer-upload-staged r))
          ;; The full pack serves every frame the incremental pack declined.
          ;; Retain it only once both uploads succeeded.
          (multiple-value-bind (packed positions) (renderer-pack-instances r)
            (renderer-count r :full-packs)
            (renderer-upload-atlas r)
            (gl-bind-vertex-array (renderer-vao r))
            (renderer-upload-instances r packed)
            (renderer-retain-pack r packed positions)))
      (dotimes (attribute 2)
        (gl-enable-vertex-attrib-array attribute)
        (gl-vertex-attrib-i-pointer attribute 4 #x1405 32 (cffi:make-pointer (* attribute 16)))
        (gl-vertex-attrib-divisor attribute 1))
      (gl-bind-framebuffer #x8D40 (renderer-fbo r))
      (gl-viewport 0 0 (renderer-width r) (renderer-height r))
      (dolist (cap '(#x0BD0 #x0BE2 #x0C11 #x0B71 #x0B44)) (gl-disable cap))
      ;; RGBA8 is linear, not an sRGB attachment; ES3 has no desktop sRGB enable.
      (renderer-color #x000000) (gl-clear #x4000)
      (gl-use-program (renderer-program r))
      (flet ((location (name) (let ((v (gl-get-uniform-location (renderer-program r) name)))
                               (when (< v 0) (error "Missing production uniform ~A" name)) v)))
        (gl-uniform-2i (location "u_target_size") (renderer-width r) (renderer-height r))
        (gl-uniform-2i (location "u_cell_size") (renderer-cw r) (renderer-ch r))
        (gl-uniform-1i (location "u_atlas_columns") (renderer-atlas-columns r))
        (gl-uniform-1i (location "u_atlas") 0))
      (let ((count (floor (length (renderer-pack-staged r)) 32))
            (runs (renderer-pack-runs r)))
        (flet ((draw (n)
                 (gl-draw-arrays-instanced #x0004 0 6 n)
                 (gl-check 'renderer-draw)
                 (renderer-count r :draw-calls) (renderer-count r :instances n)))
          (if runs
              ;; Only clipped fragments need separate scissor draws. Keep runs
              ;; of ordinary cells instanced even in a partially clipped frame.
              (loop for (index n rectangle) in runs
                    do (if rectangle
                           (destructuring-bind (x y right bottom) rectangle
                             (gl-enable #x0C11)
                             (gl-scissor x (- (renderer-height r) bottom) (- right x) (- bottom y)))
                           (gl-disable #x0C11))
                       (dotimes (attribute 2)
                         (gl-vertex-attrib-i-pointer attribute 4 #x1405 32
                                                    (cffi:make-pointer (+ (* index 32) (* attribute 16)))))
                       (draw n))
              (when (plusp count) (draw count)))))
      (gl-enable #x0C11)
      (maphash (lambda (rectangle color)
                 (destructuring-bind (x y width height) rectangle
                   (gl-scissor x (- (renderer-height r) y height) width height)
                   (renderer-color color) (gl-clear #x4000)
                   (renderer-count r :remainder-clears))) (renderer-remainders r))
      (gl-disable #x0C11)
      (gl-check 'renderer-frame)
      (renderer-count r :frames)
      (renderer-count r :submission-ticks (- (get-internal-real-time) start))
      (setf (renderer-dirty r) nil)))
  r)

(defgeneric renderer-metrics (renderer))
(defmethod renderer-metrics ((r gpu-renderer))
  (append (list :renderer :gpu :driver (context-info (renderer-context r))
                :atlas-epoch (atlas-epoch (renderer-atlas r))
                :ticks-per-second internal-time-units-per-second)
          (loop for k being the hash-keys of (renderer-counters r)
                using (hash-value v) append (list k v))))
