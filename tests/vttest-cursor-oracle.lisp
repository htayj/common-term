;;;; Independent, source-derived oracle for all six vttest 20251205 cursor pauses.
;;;; No product packages, terminal parser, renderer, or captured cell data are used.
(require :asdf)
(defpackage #:vttest-cursor-oracle
  (:use #:cl)
  (:export #:first-screen #:read-bytes #:read-data #:rasterize #:write-ppm
           #:compare-rgb #:font-info #:glyph-rows #:screen #:compare-screen #:compare-png))
(in-package #:vttest-cursor-oracle)

(defun read-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (assert (= (read-sequence v s) (length v))) v)))

(defun read-data (path)
  "One data form with exact EOF, no dispatch syntax or executable reader forms."
  (let ((*read-eval* nil) (*readtable* (copy-readtable nil)))
    (set-macro-character #\# (lambda (&rest args) (declare (ignore args))
                              (error "Dispatch syntax is not evidence data")))
    (with-open-file (s path)
      (let* ((end (gensym)) (v (read s nil end)))
        (assert (not (eq v end)))
        (assert (eq end (read s nil end)))
        (labels ((check (x)
                   (cond ((consp x) (check (car x)) (check (cdr x)))
                         ((or (null x) (eq x t) (keywordp x) (stringp x) (numberp x)))
                         (t (error "Non-data evidence atom ~s" x)))))
          (check v)) v))))

(defun be16 (v p) (+ (ash (aref v p) 8) (aref v (1+ p))))
(defun be32 (v p) (+ (ash (be16 v p) 16) (be16 v (+ p 2))))
(defun signed-byte-value (x) (if (> x 127) (- x 256) x))
(defun table-offset (font name)
  (or (loop for p from 12 below (+ 12 (* 16 (be16 font 4))) by 16
            when (string= name (map 'string #'code-char (subseq font p (+ p 4))))
              return (be32 font (+ p 8)))
      (error "Missing SFNT table ~a" name)))

(defun cmap-glyph (font code)
  "Decode the font's Unicode BMP format-4 cmap, including idRangeOffset."
  (let* ((base (table-offset font "cmap"))
         (sub (loop for i below (be16 font (+ base 2))
                    for p = (+ base 4 (* i 8))
                    when (and (= 3 (be16 font p)) (= 1 (be16 font (+ p 2))))
                      return (+ base (be32 font (+ p 4))))))
    (assert (and sub (= 4 (be16 font sub))))
    (let* ((n (/ (be16 font (+ sub 6)) 2))
           (ends (+ sub 14)) (starts (+ ends (* n 2) 2))
           (deltas (+ starts (* n 2))) (ranges (+ deltas (* n 2))))
      (or (loop for i below n for end = (be16 font (+ ends (* i 2)))
                for start = (be16 font (+ starts (* i 2)))
                when (<= start code end)
                  return (let* ((delta (be16 font (+ deltas (* i 2))))
                                (p (+ ranges (* i 2))) (offset (be16 font p)))
                           (if (zerop offset) (mod (+ code delta) 65536)
                               (let ((g (be16 font (+ p offset (* 2 (- code start))))))
                                 (if (zerop g) 0 (mod (+ g delta) 65536))))))
          (error "No Unicode mapping for ~d" code)))))

(defun font-info (font)
  "Validate the exact Terminus strike layout supported by this small oracle.
OpenType EBLC indexFormat=2, EBDT imageFormat=1: small metrics + byte-aligned rows."
  (let* ((b (table-offset font "EBLC")) (size (+ b 8))
         (array (+ b (be32 font size)))
         (index (+ array (be32 font (+ array 4)))))
    (assert (= #x20000 (be32 font b)))
    (assert (= 1 (be32 font (+ b 4)) (be32 font (+ size 8))))
    (assert (= 16 (aref font (+ size 44)) (aref font (+ size 45))))
    (assert (= 1 (aref font (+ size 46))))
    (assert (= 2 (be16 font index)))
    (assert (= 1 (be16 font (+ index 2))))
    (assert (= 21 (be32 font (+ index 8))))
    (list :first (be16 font array) :last (be16 font (+ array 2))
          :data (+ (table-offset font "EBDT") (be32 font (+ index 4)))
          :stride 21 :width 8 :height 16
          :ascent (signed-byte-value (aref font (+ size 16)))
          :descent (signed-byte-value (aref font (+ size 17))))))

(defun glyph-rows (font char)
  (let* ((info (font-info font)) (g (cmap-glyph font (char-code char)))
         (p (+ (getf info :data) (* (- g (getf info :first)) 21))))
    (assert (<= (getf info :first) g (getf info :last)))
    ;; Terminus's 8x16 strike has a full-cell bitmap at bearing (0,12), advance 8.
    ;; Its ascent=12, descent=-4, so no crop, scale or baseline adjustment is needed.
    (assert (equal '(16 8 0 12 8) (coerce (subseq font p (+ p 5)) 'list)))
    (subseq font (+ p 5) (+ p 21))))

(defun first-screen (&optional (width 80))
  "80/132x24 cells derived from main.c:tst_movements lines 320-432.
Coordinates in this function use the upstream's 1-based positions.
DECALN/ED/EL leave E at rows9..16, cols11..70. The six 58-space
writes clear rows10..15, cols12..69. CUP/CUF/CUB/IND/RI/NEL loops
form the outer star and plus border. Normal attributes throughout.
printxx emits no newline; unix_io.c:holdit appends its 13-byte prompt."
  (assert (member width '(80 132)))
  (let ((cells (make-array (list 24 width) :element-type 'character :initial-element #\Space))
        (shift (/ (- width 80) 2)))
    (labels ((put (r c char) (setf (aref cells (1- r) (1- c)) char))
             (text (r c string) (loop for ch across string for x from (+ c shift) do (put r x ch))))
      (loop for r from 9 to 16 do (loop for c from (+ 11 shift) to (+ 70 shift) do (put r c #\E)))
      (loop for r from 10 to 15 do (loop for c from (+ 12 shift) to (+ 69 shift) do (put r c #\Space)))
      (loop for c from 1 to width do (put 1 c #\*) (put 24 c #\*))
      (loop for r from 2 to 23 do
        (put r 1 #\*) (put r width #\*) (put r 2 #\+) (put r (1- width) #\+))
      (loop for c from 3 to (- width 2) do (put 2 c #\+) (put 23 c #\+))
      (text 11 13 "The screen should be cleared,  and have an unbroken bor-")
      (text 12 13 "der of *'s and +'s around the edge,   and exactly in the")
      (text 13 13 "middle  there should be a frame of E's around this  text")
      (text 14 13 "with  one (1) free position around it.    Push <RETURN>"))
    (values cells (list :row 14 :column (+ 68 shift) :bold nil :underline nil :inverse nil :blink nil))))

(defun rasterize (cells font &key (cursor-visible t) (cursor '(:row 14 :column 68)))
  "Independently rasterize EBDT bits. Colours are the capture-bound default
palette contract (framebuffer.lisp), not sampled from the screenshot."
  (assert (and (= 24 (array-dimension cells 0)) (member (array-dimension cells 1) '(80 132))))
  (let* ((columns (array-dimension cells 1)) (width (* 8 columns))
         (pixels (make-array (* width 384 3) :element-type '(unsigned-byte 8)))
         (glyphs (make-hash-table)) (fg '(235 219 178)) (bg '(29 32 33)))
    (dotimes (r 24)
      (dotimes (c columns)
        (let* ((ch (aref cells r c))
               (rows (or (gethash ch glyphs) (setf (gethash ch glyphs) (glyph-rows font ch))))
               (at-cursor (and cursor-visible (= r (1- (getf cursor :row))) (= c (1- (getf cursor :column))))))
          (dotimes (y 16)
            (dotimes (x 8)
              (let* ((ink (logbitp (- 7 x) (aref rows y)))
                     (rgb (if (if at-cursor (not ink) ink) fg bg))
                     (p (* 3 (+ (* (+ (* r 16) y) width) (* c 8) x))))
                (loop for b in rgb for i from p do (setf (aref pixels i) b))))))))
    pixels))

(defun write-ppm (path rgb &key (width 640))
  (assert (= (* width 384 3) (length rgb)))
  (with-open-file (s path :direction :output :if-exists :error :element-type '(unsigned-byte 8))
    (write-sequence (map '(vector (unsigned-byte 8)) #'char-code (format nil "P6~%~d 384~%255~%" width)) s)
    (write-sequence rgb s)))

(defun compare-rgb (expected actual &key (diff-path nil) (width 640))
  "Compare every pixel exactly. No tolerance, masks, or registration."
  (assert (= (* width 384 3) (length expected) (length actual)))
  (let ((count 0) (first nil) (cells (make-hash-table :test 'equal))
        (diff (make-array (length expected) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (pixel (* width 384))
      (let* ((p (* pixel 3))
             (same (loop for i from p below (+ p 3) always (= (aref expected i) (aref actual i)))))
        (unless same
          (incf count)
          (multiple-value-bind (y x) (floor pixel width)
            (setf (gethash (list (1+ (floor y 16)) (1+ (floor x 8))) cells) t)
            (unless first
              (setf first (list :x x :y y :row (1+ (floor y 16)) :column (1+ (floor x 8))
                                :expected (coerce (subseq expected p (+ p 3)) 'list)
                                :actual (coerce (subseq actual p (+ p 3)) 'list)))))
          (setf (aref diff p) 255))))
    (when diff-path (write-ppm diff-path diff :width width))
    (list :pass (zerop count) :compared-pixels (* width 384) :different-pixels count
          :different-cells (sort (loop for k being the hash-keys of cells collect k)
                                 (lambda (a b) (or (< (first a) (first b))
                                                  (and (= (first a) (first b)) (< (second a) (second b))))))
          :first-difference first :tolerance 0 :masked-pixels 0)))


(defun screen (pause)
  "Direct expected-cell derivations from pinned main.c:tst_movements 320..546.
All six pauses use normal attributes. No terminal stream is interpreted here."
  (assert (member pause '(1 2 3 4 5 6)))
  (when (<= pause 2) (return-from screen (first-screen (if (= pause 1) 80 132))))
  (let* ((width (if (= pause 4) 132 80))
         (cells (make-array (list 24 width) :element-type 'character :initial-element #\Space))
         (cursor-row (case pause ((3 4) 22) (5 9) (6 20))))
    (labels ((text (row col string)
               (loop for ch across string for c from col
                     do (setf (aref cells (1- row) (1- c)) ch))))
      (case pause
        ((3 4)
         ;; main.c 436..495: each iteration scrolls the 3..21 region once.
         ;; After 26 letters, I..Z occupy rows3..20, with row21 blank.
         ;; The four control variants preserve the same ordered margins.
         (text 1 1 "Test of autowrap, mixing control and print characters.")
         (text 2 1 "The left/right margins should have letters in order:")
         (loop for row from 3 to 20 for code from (char-code #\I) do
           (text row 1 (string (code-char code)))
           (text row width (string (char-downcase (code-char code))))))
        (5
         ;; main.c 503..529: BS in CUF, CR in CUF and VT in CUU
         ;; place the three derived copies at the same odd columns as row4.
         (text 1 1 "Test of cursor-control characters inside ESC sequences.")
         (text 2 1 "Below should be four identical lines:")
         (loop for row from 4 to 7 do (text row 1 "A B C D E F G H I")))
        (6
         ;; main.c 535..546: the leading-zero CUPs explicitly target row4;
         ;; CUP(20,1) precedes the menu dispatcher holdit at main.c1747.
         (text 1 1 "Test of leading zeros in ESC sequences.")
         (text 2 1 "Two lines below you should see the sentence \"This is a correct sentence\".")
         (text 4 1 "This is a correct sentence")))
      (text cursor-row 1 "Push <RETURN>"))
    (values cells (list :row cursor-row :column 14 :bold nil :underline nil :inverse nil :blink nil))))

(defun compare-screen (pause actual-rgb font &key diff-path)
  "Compare decoded input pixels for one pause against the independent oracle."
  (multiple-value-bind (cells cursor) (screen pause)
    (compare-rgb (rasterize cells font :cursor cursor) actual-rgb
                 :width (* 8 (array-dimension cells 1)) :diff-path diff-path)))


(defun compare-png (pause input-png font-path output-directory magick)
  "Compare one case identity (pause 1..6) from PNG input, using a fresh output directory.
MAGICK decodes opaque RGB only; cell semantics and font rasterization are independent.
Returns the exact comparison plus input identity and cursor metadata."
  (let* ((out (uiop:ensure-directory-pathname output-directory))
         (raw (merge-pathnames "actual.rgba" out))
         (diff (merge-pathnames "diff.ppm" out))
         (cells (screen pause)) (width (* 8 (array-dimension cells 1))))
    (assert (not (probe-file out)))
    (ensure-directories-exist raw)
    (assert (string= (format nil "~d 384" width)
                     (string-trim '(#\Space #\Newline #\Return)
                       (uiop:run-program (list magick "identify" "-format" "%w %h" input-png)
                                         :output :string))))
    (uiop:run-program (list magick input-png "-depth" "8"
                            (concatenate 'string "rgba:" (namestring raw))))
    (let* ((rgba (read-bytes raw))
           (rgb (make-array (* width 384 3) :element-type '(unsigned-byte 8))))
      (assert (= (* width 384 4) (length rgba)))
      (assert (loop for p from 3 below (length rgba) by 4 always (= 255 (aref rgba p))))
      (dotimes (p (* width 384))
        (dotimes (i 3) (setf (aref rgb (+ (* p 3) i)) (aref rgba (+ (* p 4) i)))))
      (list :pause pause :input input-png :font font-path
            :cursor (nth-value 1 (screen pause))
            :comparison (compare-screen pause rgb (read-bytes font-path) :diff-path diff)))))
