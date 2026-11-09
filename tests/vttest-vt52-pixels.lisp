;;;; Exact expected raster and per-cell comparator for the VT52 hold. Synthetic
;;;; foundation: no capture or native admission. Glyphs: admitted-glyph-rows;
;;;; tiles: vttest-screen-oracle:cell-rgba. Rows and columns are one-based.
(load (merge-pathnames "vttest-vt52-oracle.lisp" *load-truename*))
(load (merge-pathnames "vttest-vt52-sha256.lisp" *load-truename*))

(defpackage #:vttest-vt52-pixels
  (:use #:cl)
  (:local-nicknames (#:o #:vttest-vt52-oracle) (#:s #:vttest-screen-oracle))
  (:export #:+font-sha256+ #:+font-length+ #:cursor-cell #:expected-rgba
           #:named-groups #:compare-frames
           #:pixel-refusal #:pixel-refusal-field #:pixel-refusal-value))
(in-package #:vttest-vt52-pixels)

(defparameter +font-sha256+
  "0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca")
(defparameter +font-length+ 38848)

(define-condition pixel-refusal (error)
  ((field :initarg :field :reader pixel-refusal-field)
   (value :initarg :value :reader pixel-refusal-value))
  (:report (lambda (c stream)
             (format stream "VT52 pixel input refused: ~s = ~s"
                     (pixel-refusal-field c) (pixel-refusal-value c)))))

(defun need (ok field value)
  (unless ok (error 'pixel-refusal :field field :value value)))

(defun cursor-cell (snapshot)
  "One-based (row column) of the visible cursor named by the snapshot metadata."
  (let* ((meta (o:snapshot-metadata snapshot)) (cursor (getf meta :cursor)))
    (need (and (typep (getf cursor :row) '(integer 1 24))
               (typep (getf cursor :column) '(integer 1 80))
               (eq t (getf cursor :visible)) (null (getf meta :screen-reverse)))
          :cursor cursor)
    (list (getf cursor :row) (getf cursor :column))))

(defun expected-rgba (snapshot font &key font-sha256 (scale nil scale-p)
                                      (blink-phase nil phase-p))
  "Opaque RGBA8 640x384 frame expanded by exact SCALE (1 or 2) replication.
The observed BLINK-PHASE is mandatory; T inverts the metadata cursor cell.
FONT must hash to FONT-SHA256, the pin."
  (need (and scale-p (member scale '(1 2))) :scale scale)
  (need (and phase-p (member blink-phase '(t nil))) :blink-phase blink-phase)
  (need (equal font-sha256 +font-sha256+) :font-sha256 font-sha256)
  (need (and (typep font '(vector (unsigned-byte 8)))
             (equal +font-sha256+ (vttest-vt52-sha256:sha256-hex font)))
        :font (if (vectorp font) (length font) (type-of font)))
  (destructuring-bind (cursor-row cursor-column) (cursor-cell snapshot)
    (let ((image (make-array (* 640 384 4) :element-type '(unsigned-byte 8)))
          (glyphs (make-hash-table)))
      (dotimes (r 24)
        (dotimes (c 80)
          (let* ((cell (o:snapshot-cell snapshot (1+ r) (1+ c)))
                 (code (s:cell-code cell))
                 (rows (or (gethash code glyphs)
                           (setf (gethash code glyphs)
                                 (vttest-charsets-oracle:admitted-glyph-rows font code))))
                 (tile (s:cell-rgba cell rows :blink-phase blink-phase :screen-reverse nil
                                    :cursor (and blink-phase (= (1+ r) cursor-row)
                                                 (= (1+ c) cursor-column)))))
            (dotimes (y 16)
              (replace image tile :start1 (* 4 (+ (* (+ (* r 16) y) 640) (* c 8)))
                                  :start2 (* y 32) :end2 (* (1+ y) 32))))))
      (values (vttest-cursor-oracle:scale-raster image 640 384 scale)))))

(defun named-groups (snapshot)
  "Plist :graphics (o:graphics-cells) plus o:boundary-cells classified by geometry
into :left :right :above :below and :comparison (cells above the run whose code
equals the same column's graphics byte). Unclassified or incomplete is refused."
  (let* ((graphics (o:graphics-cells snapshot))
         (row (getf (first graphics) :row))
         (columns (mapcar (lambda (g) (getf g :column)) graphics))
         (low (reduce #'min columns)) (high (reduce #'max columns))
         (groups (list :left nil :right nil :above nil :below nil :comparison nil)))
    (need (and (= 32 (length graphics))
               (every (lambda (g) (eql row (getf g :row))) graphics)
               (equal columns (loop for c from low to high collect c)))
          :graphics columns)
    (dolist (cell (o:boundary-cells))
      (destructuring-bind (r c) cell
        (let* ((code (s:cell-code (o:snapshot-cell snapshot r c)))
               (same (find c graphics :key (lambda (g) (getf g :column))))
               (group (cond ((and (= r row) (= c (1- low))) :left)
                            ((and (= r row) (= c (1+ high))) :right)
                            ((and (= r (1- row)) (<= (1- low) c (1+ high))) :above)
                            ((and (= r (1+ row)) (<= (1- low) c (1+ high))) :below)
                            ((and same (< r (1- row)) (eql code (getf same :byte)))
                             :comparison))))
          (need group :boundary-cell cell)
          (need (notany (lambda (e) (eql c (getf e :column))) (getf groups group))
                :duplicate-cell cell)
          (push (append (list :row r :column c :code code)
                        (and (eq group :comparison) (list :byte code)))
                (getf groups group)))))
    (flet ((span (group)
             (sort (mapcar (lambda (e) (getf e :column)) (getf groups group)) #'<)))
      (need (and (equal (span :left) (list (1- low)))
                 (equal (span :right) (list (1+ high)))
                 (equal (span :above) (loop for c from (1- low) to (1+ high) collect c))
                 (equal (span :below) (span :above))
                 (equal (span :comparison) columns))
            :boundary-coverage (list :low low :high high)))
    (list* :graphics graphics
           (loop for (key entries) on groups by #'cddr
                 nconc (list key (sort (copy-list entries) #'<
                                       :key (lambda (e) (getf e :column))))))))

(defun compare-frames (snapshot expected actual &key (scale nil scale-p))
  "Exact full-frame RGBA8 comparison at SCALE (1 or 2), no tolerance, masks or
resampling. Named cells carry :DIFFERENT-PIXELS; :OUTSIDE-NAMED-PIXELS is the rest."
  (need (and scale-p (member scale '(1 2))) :scale scale)
  (let* ((width (* 640 scale)) (height (* 384 scale)) (size (* width height 4))
         (counts (make-array 1920 :initial-element 0)) (total 0))
    (loop for (field frame) in (list (list :expected expected) (list :actual actual)) do
      (need (and (typep frame '(vector (unsigned-byte 8))) (= size (length frame)))
            field (if (vectorp frame) (length frame) (type-of frame)))
      (need (loop for p from 3 below size by 4 always (= 255 (aref frame p)))
            field :nonopaque))
    (dotimes (y height)
      (dotimes (x width)
        (let ((p (* 4 (+ x (* y width)))))
          (unless (loop for i from p below (+ p 4) always (= (aref expected i) (aref actual i)))
            (incf total)
            (incf (aref counts (+ (* 80 (floor y (* 16 scale))) (floor x (* 8 scale)))))))))
    (let* ((named 0)
           (groups
             (loop for (key entries) on (named-groups snapshot) by #'cddr
                   nconc (list key
                               (loop for e in entries
                                     for n = (aref counts (+ (* 80 (1- (getf e :row)))
                                                             (1- (getf e :column))))
                                     do (incf named n)
                                     collect (append e (list :different-pixels n
                                                             :pass (zerop n))))))))
      (list* :pass (zerop total) :scale scale :width width :height height
             :compared-pixels (* width height) :different-pixels total
             :outside-named-pixels (- total named)
             :different-cells (loop for i below 1920 unless (zerop (aref counts i))
                                    collect (multiple-value-bind (r c) (floor i 80)
                                              (list (1+ r) (1+ c))))
             :tolerance 0 :masked-pixels 0
             groups))))
