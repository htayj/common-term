;;;; Offline compare-physical controls; loads only the oracle source.
(require :asdf)
(load (merge-pathnames "vttest-cursor-oracle.lisp" (or *load-truename* *default-pathname-defaults*)))
(defpackage #:vttest-physical-comparison-tests (:use #:cl #:vttest-cursor-oracle))
(in-package #:vttest-physical-comparison-tests)

(defvar *checks* 0)
(defun check (ok name) (assert ok () "Failed ~s" name) (incf *checks*))
(defun refusal (thunk)
  (handler-case (progn (funcall thunk) nil)
    (physical-comparison-refusal (c)
      (list (physical-refusal-reason c) (physical-refusal-details c)))))
(defun refuses (reason thunk name) (check (eq reason (first (refusal thunk))) name))

(defun raster (w h colour)
  (let ((v (make-array (* w h 4) :element-type '(unsigned-byte 8))))
    (dotimes (y h v)
      (dotimes (x w)
        (let ((p (* 4 (+ x (* w y)))))
          (replace v (funcall colour x y) :start1 p)
          (setf (aref v (+ p 3)) 255))))))
(defun pattern (x y) (list (mod (+ x (* 3 y)) 256) (mod (* 5 y) 256) (mod (logxor x y) 256)))
(defun stripe (x y) (declare (ignore y)) (if (= x 10) '(235 219 178) '(29 32 33)))
(defun physical (logical w h s) (copy-seq (scale-raster logical w h s :channels 4)))
(defun pixel (v pw x y) (let ((p (* 4 (+ x (* pw y))))) (coerce (subseq v p (+ p 4)) 'list)))

(defun naive-count (logical actual w h s)
  (let ((pw (* w s)) (count 0))
    (dotimes (y (* h s) count)
      (dotimes (x pw)
        (let ((e (* 4 (+ (floor x s) (* w (floor y s))))) (a (* 4 (+ x (* pw y)))))
          (unless (loop for i below 4 always (= (aref logical (+ e i)) (aref actual (+ a i))))
            (incf count)))))))

(defun consistent (r)
  (let ((n (getf r :different-pixels)))
    (and (equal (loop for k in r by #'cddr collect k)
                '(:pass :scale :width :height :compared-pixels :different-pixels
                  :different-cells :first-difference :last-difference :tolerance :masked-pixels))
         (eq (getf r :pass) (zerop n)) (<= 0 n (getf r :compared-pixels))
         (= (getf r :compared-pixels) (* (getf r :width) (getf r :height)))
         (eql 0 (getf r :tolerance)) (eql 0 (getf r :masked-pixels))
         (eq (zerop n) (null (getf r :different-cells)))
         (eq (zerop n) (null (getf r :first-difference)))
         (eq (zerop n) (null (getf r :last-difference))))))
(defun compare (e a w s)
  (let ((r (compare-physical e a w 384 s))) (check (consistent r) (list :consistent w s)) r))

;; Identity at both scales and widths.
(dolist (c '((1 640 245760) (1 1056 405504) (2 640 983040) (2 1056 1622016)))
  (destructuring-bind (s w pixels) c
    (let* ((e (raster w 384 #'pattern)) (r (compare e (physical e w 384 s) w s)))
      (check (and (getf r :pass) (= s (getf r :scale)) (= (* s w) (getf r :width))
                  (= (* s 384) (getf r :height)) (= pixels (getf r :compared-pixels))
                  (= 0 (getf r :different-pixels)))
             (list :identity s w)))))

;; One changed red byte counts one.
(dolist (c '((1 640 3 5 3 5 1 1) (2 640 3 5 1 2 1 1) (2 1056 2111 767 1055 383 24 132)))
  (destructuring-bind (s w x y lx ly row column) c
    (let* ((e (raster w 384 #'pattern)) (a (physical e w 384 s)) (pw (* s w))
           (p (* 4 (+ x (* pw y)))) (before (pixel a pw x y)))
      (setf (aref a p) (logxor 255 (aref a p)))
      (let* ((r (compare e a w s)) (d (getf r :first-difference)))
        (check (and (= 1 (getf r :different-pixels) (naive-count e a w 384 s))
                    (equal d (getf r :last-difference))
                    (equal d (list :x x :y y :logical-x lx :logical-y ly :row row :column column
                                   :expected before :actual (pixel a pw x y)))
                    (equal (list (list row column)) (getf r :different-cells)))
               (list :single s w x y))))))

(defun shifted (v pw ph)
  (let ((a (copy-seq v)))
    (dotimes (y ph a)
      (loop for x from 1 below pw for p = (* 4 (+ x (* pw y)))
            do (replace a v :start1 p :start2 (- p 4) :end2 p)))))

;; One-pixel shift against the naive count.
(dolist (s '(1 2))
  (let* ((e (raster 640 384 #'pattern)) (a (shifted (physical e 640 384 s) (* s 640) (* s 384))))
    (check (= (naive-count e a 640 384 s) (getf (compare e a 640 s) :different-pixels))
           (list :shift s))))

;; Logical x=10 -> physical 20..21, shifted 21..22: x=20,22 differ on 768 rows.
(let* ((e (raster 640 384 #'stripe)) (a (shifted (physical e 640 384 2) 1280 768))
       (r (compare e a 640 2)))
  (check (= 1536 (* 2 768) (naive-count e a 640 384 2) (getf r :different-pixels)) :stripe-count)
  (check (equal '(:x 20 :y 0) (subseq (getf r :first-difference) 0 4)) :stripe-first)
  (check (equal '(:x 22 :y 767) (subseq (getf r :last-difference) 0 4)) :stripe-last)
  (check (equal (loop for row from 1 to 24 collect (list row 2)) (getf r :different-cells))
         :stripe-cells))

;; Blended fg/bg pixel.
(let* ((e (raster 640 384 #'stripe)) (a (physical e 640 384 2)))
  (replace a '(132 126 106 255) :start1 (* 4 (+ 22 (* 1280 7))))
  (let ((r (compare e a 640 2)))
    (check (and (= 1 (getf r :different-pixels) (naive-count e a 640 384 2))
                (equal (getf r :first-difference)
                       '(:x 22 :y 7 :logical-x 11 :logical-y 3 :row 1 :column 2
                         :expected (29 32 33 255) :actual (132 126 106 255))))
           :blended)))

;; Alpha refusals.
(let ((e (raster 8 16 #'pattern)))
  (dolist (alpha '(254 0))
    (let ((a (physical e 8 16 2)))
      (setf (aref a (+ 3 (* 4 (+ 3 (* 16 5))))) alpha)
      (check (equal (list :actual-alpha (list :x 3 :y 5 :alpha alpha))
                    (refusal (lambda () (compare-physical e a 8 16 2))))
             (list :actual-alpha alpha))))
  (let ((bad (copy-seq e)))
    (setf (aref bad (+ 3 (* 4 (+ 3 (* 8 5))))) 254)
    (check (equal '(:expected-alpha (:x 3 :y 5 :alpha 254))
                  (refusal (lambda () (compare-physical bad (physical e 8 16 2) 8 16 2))))
           :expected-alpha)))

;; Length and type refusals.
(let* ((e (raster 8 16 #'pattern)) (a (physical e 8 16 2)) (n (length a)))
  (check (equal (list :actual-raster (list :expected-length n :actual-length (- n 4)))
                (refusal (lambda () (compare-physical e (subseq a 4) 8 16 2))))
         :actual-off-by-four)
  (loop for bad in (list (subseq a 1) (concatenate '(vector (unsigned-byte 8)) a '(255))
                         (copy-seq e) (coerce a 'simple-vector) nil)
        for i from 0
        do (refuses :actual-raster (lambda () (compare-physical e bad 8 16 2)) (list :actual i)))
  (refuses :expected-raster (lambda () (compare-physical a a 8 16 2)) :expected-physical-sized)
  (refuses :expected-raster (lambda () (compare-physical (coerce e 'simple-vector) a 8 16 2))
           :expected-type))

;; Flat vectors carry no shape: swapped dimensions of equal length follow the
;; supplied dimensions; only the dimension/length contract is checked.
(let* ((e (raster 3 2 #'pattern)) (r (compare-physical e (physical e 3 2 2) 2 3 2)))
  (check (and (consistent r) (= 4 (getf r :width)) (= 6 (getf r :height))) :swapped-contract)
  (refuses :expected-raster (lambda () (compare-physical e (physical e 3 2 2) 3 3 2)) :area))

;; Scale, dimensions and refusal order.
(let ((e (raster 3 2 #'pattern)))
  (dolist (s '(0 3 "2" 2.0 nil -1 1/2))
    (refuses :scale (lambda () (compare-physical e e 3 2 s)) (list :scale s)))
  (dolist (wh '((0 2) (3 0) (-3 2) (3.0 2) (3 nil) (3/2 2)))
    (refuses :logical-dimensions (lambda () (compare-physical e e (first wh) (second wh) 1))
             (list :dimensions wh)))
  (refuses :scale (lambda () (compare-physical nil nil 0 0 3)) :order-scale)
  (refuses :logical-dimensions (lambda () (compare-physical nil nil 0 2 1)) :order-dimensions)
  (refuses :expected-raster (lambda () (compare-physical nil nil 3 2 1)) :order-expected)
  (let ((bad (copy-seq e)))
    (setf (aref bad 3) 0)
    (refuses :expected-alpha (lambda () (compare-physical bad nil 3 2 1)) :order-alpha)
    (refuses :actual-raster (lambda () (compare-physical e (subseq bad 4) 3 2 1)) :order-actual)))

(format t "compare-physical controls passed: ~d checks~%" *checks*)
