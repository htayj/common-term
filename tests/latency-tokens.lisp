;;;; Token text and placement shared by the probe child and the measurement side.
;;;; Loads under the packaged SBCL alone; it depends on nothing else.
(defpackage #:lispterm.latency.tokens
  (:use #:cl)
  (:export #:+token-length+ #:+token-column+ #:token-text #:token-row #:token-bytes
           #:ready-text #:ready-bytes #:key-byte #:ascii-octets))
(in-package #:lispterm.latency.tokens)

(defconstant +token-length+ 5)
(defconstant +token-column+ 4 "Zero-based column of every token and of the ready marker.")

(defun token-text (nonce index)
  "Five visible characters unique in a run: the two-character NONCE and INDEX in base 36."
  (check-type index (integer 0 46655))
  (format nil "~A~36,3,'0R" nonce index))

(defun token-row (index slots)
  "Zero-based row of token INDEX. Row 0 holds the ready marker and the parked cursor."
  (1+ (mod index slots)))

(defun ready-text (nonce) (concatenate 'string nonce "RDY"))

(defun ascii-octets (string)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))

(defun place (text row)
  "TEXT at ROW and the token column by absolute position, cursor parked at the origin."
  (ascii-octets (format nil "~C[~D;~DH~A~C[1;1H" #\Esc (1+ row) (1+ +token-column+) text #\Esc)))

(defun token-bytes (nonce index slots)
  (place (token-text nonce index) (token-row index slots)))

(defun ready-bytes (nonce)
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (ascii-octets (format nil "~C[2J" #\Esc)) (place (ready-text nonce) 0)))

(defun key-byte (index)
  "The byte the injector sends for key INDEX; the probe checks it."
  (+ (char-code #\a) (mod index 26)))
