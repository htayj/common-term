;;;; Lispterm font-otb. Native Lisp display implementation.

(in-package #:lispterm.display)

(defun be16 (v p) (+ (ash (aref v p) 8) (aref v (1+ p))))

(defun be32 (v p) (+ (ash (be16 v p) 16) (be16 v (+ p 2))))

(defun signed8 (n)
  (if (> n 127)
      (- n 256)
      n))

(defstruct bitmap-font
  width
  height
  ascent
  bytes
  cmap-offset
  eblc
  ebdt
  (cache (make-hash-table))
  ;; Namestring READ-OTB opened, or nil for a font built directly.
  (path nil))

(defun font-width (f) (bitmap-font-width f))

(defun font-height (f) (bitmap-font-height f))

(defun read-otb (path)
  (let ((v
          (with-open-file (in path :element-type '(unsigned-byte 8))
            (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
              (read-sequence v in)
              v))))
    (parse-otb v path)))

(defun parse-otb (v &optional path)
  "Decode the OTB octets V, kept as the font's bytes. PATH, if given, is recorded."
  (let* ((tables (make-hash-table :test 'equal)))
    (dotimes (i (be16 v 4))
      (let ((p (+ 12 (* i 16))))
        (setf (gethash (map 'string #'code-char (subseq v p (+ p 4))) tables)
                (be32 v (+ p 8)))))
    (let* ((b (gethash "EBLC" tables)) (s (+ b 8)) (cm (gethash "cmap" tables)) (cmap nil))
      (dotimes (i (be16 v (+ cm 2)))
        (let* ((p (+ cm 4 (* i 8))) (o (+ cm (be32 v (+ p 4)))))
          (when (= 4 (be16 v o)) (setf cmap o))))
      (unless cmap (error "Font requires a format 4 Unicode cmap"))
      (make-bitmap-font :bytes v :width (aref v (+ s 18)) :height (aref v (+ s 45)) :ascent
       (signed8 (aref v (+ s 16))) :cmap-offset cmap :eblc b :ebdt (gethash "EBDT" tables)
       :path (and path (namestring path))))))

(defun font-glyph-index (font code)
  (let* ((v (bitmap-font-bytes font))
         (p (bitmap-font-cmap-offset font))
         (n (/ (be16 v (+ p 6)) 2))
         (ends (+ p 14))
         (starts (+ ends (* n 2) 2))
         (deltas (+ starts (* n 2)))
         (ranges (+ deltas (* n 2))))
    (dotimes (i n 0)
      (when (<= code (be16 v (+ ends (* i 2))))
        (return
         (if (< code (be16 v (+ starts (* i 2))))
             0
             (let* ((delta (be16 v (+ deltas (* i 2))))
                    (r (+ ranges (* i 2)))
                    (offset (be16 v r)))
               (if (zerop offset)
                   (mod (+ code delta) 65536)
                   (let ((g (be16 v (+ r offset (* 2 (- code (be16 v (+ starts (* i 2)))))))))
                     (if (zerop g)
                         0
                         (mod (+ g delta) 65536)))))))))))

(defun otb-glyph (font char)
  (let* ((v (bitmap-font-bytes font))
         (gid (font-glyph-index font (char-code char)))
         (s (+ (bitmap-font-eblc font) 8))
         (array (+ (bitmap-font-eblc font) (be32 v s)))
         (out
          (make-array (* (font-width font) (font-height font)) :element-type 'bit
                      :initial-element 0)))
    (dotimes (i (be32 v (+ s 8)))
      (let* ((a (+ array (* i 8))) (first (be16 v a)) (last (be16 v (+ a 2))))
        (when (<= first gid last)
          (let* ((p (+ array (be32 v (+ a 4))))
                 (index-format (be16 v p))
                 (image-format (be16 v (+ p 2)))
                 (offset
                  (ecase index-format
                    (1 (be32 v (+ p 8 (* 4 (- gid first)))))
                    (2 (* (- gid first) (be32 v (+ p 8))))
                    (3 (be16 v (+ p 8 (* 2 (- gid first)))))))
                 (q (+ (bitmap-font-ebdt font) (be32 v (+ p 4)) offset))
                 (height
                  (if (= image-format 5)
                      (aref v (+ p 12))
                      (aref v q)))
                 (width
                  (if (= image-format 5)
                      (aref v (+ p 13))
                      (aref v (+ q 1))))
                 (left
                  (signed8
                   (if (= image-format 5)
                       (aref v (+ p 14))
                       (aref v (+ q 2)))))
                 (top
                  (- (bitmap-font-ascent font)
                     (signed8
                      (if (= image-format 5)
                          (aref v (+ p 15))
                          (aref v (+ q 3))))))
                 (bits
                  (+ q
                     (if (= image-format 5)
                         0
                         5))))
            (unless (member image-format '(1 2 5))
              (error "Unsupported EBDT image format ~d" image-format))
            (dotimes (y height)
              (dotimes (x width)
                (let* ((bit
                        (if (= image-format 1)
                            (+ (* y (* 8 (ceiling width 8))) x)
                            (+ (* y width) x)))
                       (dx (+ left x))
                       (dy (+ top y)))
                  (when
                      (and (<= 0 dx) (< dx (font-width font)) (<= 0 dy)
                           (< dy (font-height font)))
                    (setf (aref out (+ dx (* dy (font-width font))))
                            (ldb (byte 1 (- 7 (mod bit 8)))
                                 (aref v (+ bits (floor bit 8)))))))))
            (return)))))
    out))
