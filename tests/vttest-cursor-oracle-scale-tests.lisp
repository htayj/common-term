;;;; Focused offline controls for vttest-cursor-oracle:scale-raster and the
;;;; height-parameterized exact comparator. Loads only the oracle source.
(require :asdf)
(load (merge-pathnames "vttest-cursor-oracle.lisp" (or *load-truename* *default-pathname-defaults*)))
(defpackage #:vttest-cursor-oracle-scale-tests (:use #:cl #:vttest-cursor-oracle))
(in-package #:vttest-cursor-oracle-scale-tests)

(defvar *checks* 0)
(defun check (ok name) (assert ok () "Failed ~a" name) (incf *checks*))
(defun rejects (thunk name)
  (check (handler-case (progn (funcall thunk) nil) (error () t)) name))
(defun bytes (list) (make-array (length list) :element-type '(unsigned-byte 8) :initial-contents list))
(defun ramp (n) (let ((v (make-array n :element-type '(unsigned-byte 8))))
                  (dotimes (i n v) (setf (aref v i) (mod (* 7 (1+ i)) 256)))))

;; Scale one is the identity, including the object itself.
(let ((v (ramp (* 3 2 4))))
  (multiple-value-bind (out w h) (scale-raster v 3 2 1)
    (check (and (eq out v) (= w 3) (= h 2)) :scale-one-identity)))

;; Scale two replicates each logical pixel, all four channels, into a 2x2 block.
(let ((v (ramp (* 3 2 4))))
  (multiple-value-bind (out w h) (scale-raster v 3 2 2)
    (check (and (= w 6) (= h 4) (= (length out) (* 6 4 4))) :scale-two-dimensions)
    (dotimes (y 4)
      (dotimes (x 6)
        (dotimes (i 4)
          (check (= (aref out (+ i (* 4 (+ x (* 6 y)))))
                    (aref v (+ i (* 4 (+ (floor x 2) (* 3 (floor y 2)))))))
                 (list :replicated x y i)))))))

;; Alpha is copied exactly, never forced opaque.
(multiple-value-bind (out) (scale-raster (bytes '(1 2 3 0 4 5 6 128)) 2 1 2)
  (check (equalp out (bytes '(1 2 3 0 1 2 3 0 4 5 6 128 4 5 6 128
                              1 2 3 0 1 2 3 0 4 5 6 128 4 5 6 128)))
         :alpha-replicated))

;; Every physical pixel and channel is compared: a one-channel change at any
;; physical coordinate, even or odd, is exactly one differing pixel there.
(let ((expected (scale-raster (ramp (* 3 2 3)) 3 2 2 :channels 3)))
  (dotimes (y 4)
    (dotimes (x 6)
      (dotimes (i 3)
        (let ((actual (copy-seq expected)) (p (+ i (* 3 (+ x (* 6 y))))))
          (setf (aref actual p) (logxor 1 (aref actual p)))
          (let ((r (compare-rgb expected actual :width 6 :height 4)))
            (check (and (not (getf r :pass)) (= 24 (getf r :compared-pixels))
                        (= 1 (getf r :different-pixels))
                        (= x (getf (getf r :first-difference) :x))
                        (= y (getf (getf r :first-difference) :y)))
                   (list :sensitive x y i))))))))

;; Full physical captures: 80 columns 640x384->1280x768, 132 columns
;; 1056x384->2112x768, all physical pixels counted, odd corner detected.
(dolist (case '((640 1280 983040) (1056 2112 1622016)))
  (destructuring-bind (lw pw count) case
    (let ((expected (scale-raster (ramp (* lw 384 3)) lw 384 2 :channels 3)))
      (multiple-value-bind (same w h) (scale-raster (ramp (* lw 384 3)) lw 384 2 :channels 3)
        (check (and (= w pw) (= h 768)) (list :physical-dimensions lw))
        (let ((r (compare-rgb expected same :width w :height h)))
          (check (and (getf r :pass) (= count (getf r :compared-pixels))) (list :physical-pass lw)))
        (let ((p (* 3 (+ (1- w) (* w (1- h))))))
          (setf (aref same (+ p 2)) (logxor 255 (aref same (+ p 2))))
          (let ((r (compare-rgb expected same :width w :height h)))
            (check (and (= 1 (getf r :different-pixels))
                        (= (1- w) (getf (getf r :first-difference) :x))
                        (= 767 (getf (getf r :first-difference) :y)))
                   (list :physical-odd-corner lw))))))))

;; Default height 384 is unchanged for logical callers.
(let ((v (ramp (* 640 384 3))))
  (check (= (* 640 384) (getf (compare-rgb v (copy-seq v)) :compared-pixels)) :default-height))

;; Strict scale, dimension, channel and length validation.
(let ((v (ramp 24)))
  (dolist (s '(0 3 -1 1.0 2.0 nil 1/2))
    (rejects (lambda () (scale-raster v 3 2 s)) (list :scale s)))
  (dolist (wh '((0 2) (3 0) (-3 -2) (3.0 2) (3 nil) (2 2) (4 2)))
    (rejects (lambda () (scale-raster v (first wh) (second wh) 2)) (list :dimensions wh)))
  (dolist (c '(1 2 5 0))
    (rejects (lambda () (scale-raster v 3 2 2 :channels c)) (list :channels c)))
  (rejects (lambda () (scale-raster (coerce v 'simple-vector) 3 2 2)) :element-type)
  (rejects (lambda () (scale-raster (subseq v 1) 3 2 1)) :short-scale-one))
(let ((v (ramp 72)))
  (rejects (lambda () (compare-rgb v v :width 6 :height 0)) :compare-zero-height)
  (rejects (lambda () (compare-rgb v (subseq v 3) :width 6 :height 4)) :compare-length)
  (rejects (lambda () (compare-rgb v v :width 6 :height 3)) :compare-height-mismatch)
  (rejects (lambda () (write-ppm "unused.ppm" v :width 6 :height 3)) :ppm-height-mismatch))

;; Height-parameterized PPM header and payload.
(uiop:with-temporary-file (:pathname path :keep nil)
  (delete-file path)
  (write-ppm path (ramp 72) :width 6 :height 4)
  (check (equalp (read-bytes path)
                 (concatenate '(vector (unsigned-byte 8)) (map 'vector #'char-code (format nil "P6~%6 4~%255~%"))
                              (ramp 72)))
         :ppm-height))

(format t "scale-raster controls passed: ~d checks~%" *checks*)
