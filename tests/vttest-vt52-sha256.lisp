;;;; Pure SHA-256 (FIPS 180-4) over an octet vector, for pinned-input admission.
;;;; Split from vttest-vt52-pixels.lisp to keep each handwritten file small.
;;;; H0 and K are the first 32 fraction bits of the square and cube roots of
;;;; the first primes, derived with exact integer roots instead of transcribed.
(defpackage #:vttest-vt52-sha256
  (:use #:cl)
  (:export #:sha256-hex))
(in-package #:vttest-vt52-sha256)

(defun root-bits (prime k)
  "Low 32 bits of floor(PRIME^(1/K) * 2^32)."
  (let* ((n (* prime (expt 2 (* 32 k))))
         (x (ash 1 (ceiling (integer-length n) k))))
    (loop for y = (floor (+ (* (1- k) x) (floor n (expt x (1- k)))) k)
          while (< y x) do (setf x y))
    (ldb (byte 32 0) x)))

(defparameter *primes*
  (loop with found = '() for n from 2 while (< (length found) 64)
        when (notany (lambda (p) (zerop (mod n p))) found)
          do (setf found (append found (list n)))
        finally (return found)))
(defparameter *h0* (map 'vector (lambda (p) (root-bits p 2)) (subseq *primes* 0 8)))
(defparameter *k* (map 'vector (lambda (p) (root-bits p 3)) *primes*))

(defun sha256-hex (octets)
  "Lowercase hexadecimal SHA-256 of a vector of octets."
  (check-type octets (vector (unsigned-byte 8)))
  (flet ((rotr (x n) (logior (ash x (- n)) (ldb (byte 32 0) (ash x (- 32 n)))))
         (add (&rest xs) (ldb (byte 32 0) (reduce #'+ xs))))
    (let* ((n (length octets)) (size (* 64 (ceiling (+ n 9) 64)))
           (m (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
           (h (copy-seq *h0*)) (w (make-array 64)))
      (replace m octets)
      (setf (aref m n) #x80)
      (dotimes (i 8) (setf (aref m (- size 1 i)) (ldb (byte 8 (* 8 i)) (* 8 n))))
      (loop for base from 0 below size by 64 do
        (dotimes (i 64)
          (setf (aref w i)
                (if (< i 16)
                    (loop for j below 4
                          sum (ash (aref m (+ base (* 4 i) j)) (* 8 (- 3 j))))
                    (let ((a (aref w (- i 15))) (b (aref w (- i 2))))
                      (add (aref w (- i 16)) (aref w (- i 7))
                           (logxor (rotr a 7) (rotr a 18) (ash a -3))
                           (logxor (rotr b 17) (rotr b 19) (ash b -10)))))))
        (let ((v (copy-seq h)))
          (dotimes (i 64)
            (destructuring-bind (a b c d e f g hh) (coerce v 'list)
              (let ((t1 (add hh (logxor (rotr e 6) (rotr e 11) (rotr e 25))
                             (logxor (logand e f) (logand (lognot e) g))
                             (aref *k* i) (aref w i)))
                    (t2 (add (logxor (rotr a 2) (rotr a 13) (rotr a 22))
                             (logxor (logand a b) (logand a c) (logand b c)))))
                (setf v (vector (add t1 t2) a b c (add d t1) e f g)))))
          (dotimes (i 8) (setf (aref h i) (add (aref h i) (aref v i))))))
      (format nil "~(~{~8,'0x~}~)" (coerce h 'list)))))
