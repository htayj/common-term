;;;; Exact double-size physical comparison and capture recording.
(defpackage #:vttest-double-size-pixels
  (:use #:cl)
  (:export #:compare-capture #:compare-memory))
(in-package #:vttest-double-size-pixels)

(defparameter +font+
  "/nix/store/9qvd27vg12177prxcl778rdhxh4lk2mi-terminus-font-4.49.1/share/fonts/terminus/ter-u16n.otb")
(defparameter +font-sha+ "0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca")
(defparameter +decoder+
  "/nix/store/h74lg8wfwndnf5glr171kmcshlffi7l2-imagemagick-7.1.2-29/bin/magick")
(defparameter +decoder-sha+ "054c52b583c303bca2aae9fadcfd9e09fbc946f83e6621b39f22650d28d3027e")
(defun octets (path)
  (vttest-double-size-native-contract::regular-file path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (unless (= (length bytes) (read-sequence bytes s)) (error "Short image/font read"))
      bytes)))

;;;; FIPS180-4 vector hashing, from real-font closure362 runner2.lisp20..56.
(defun u32 (x) (ldb (byte 32 0) x))
(defun rotate32 (x n) (u32 (logior (ash x (- n)) (ash x (- 32 n)))))
(defparameter *sha-k*
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
    #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
    #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))
(defun font-digest (octets)
  (let* ((len (length octets)) (size (* 64 (ceiling (+ len 9) 64)))
         (padded (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
         (h (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
         (w (make-array 64)))
    (replace padded octets) (setf (aref padded len) 128)
    (dotimes (i 8) (setf (aref padded (+ (- size 8) i)) (ldb (byte 8 (* 8 (- 7 i))) (* len 8))))
    (loop for base from 0 below size by 64 do
      (dotimes (i 16)
        (setf (aref w i) (loop for j below 4 for p = (+ base (* i 4) j) sum (ash (aref padded p) (* 8 (- 3 j))))))
      (loop for i from 16 below 64 for x = (aref w (- i 15)) for y = (aref w (- i 2)) do
        (setf (aref w i) (u32 (+ (aref w (- i 16))
          (logxor (rotate32 x 7) (rotate32 x 18) (ash x -3)) (aref w (- i 7))
          (logxor (rotate32 y 17) (rotate32 y 19) (ash y -10))))))
      (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
            (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
        (dotimes (i 64)
          (let* ((t1 (u32 (+ hh (logxor (rotate32 e 6) (rotate32 e 11) (rotate32 e 25))
                               (logxor (logand e f) (logand (lognot e) g)) (aref *sha-k* i) (aref w i))))
                 (t2 (u32 (+ (logxor (rotate32 a 2) (rotate32 a 13) (rotate32 a 22))
                              (logxor (logand a b) (logand a c) (logand b c))))))
            (setf hh g g f f e e (u32 (+ d t1)) d c c b b a a (u32 (+ t1 t2)))))
        (loop for value in (list a b c d e f g hh) for i from 0 do
          (setf (aref h i) (u32 (+ (aref h i) value))))))
    (string-downcase (format nil "~{~8,'0x~}" (coerce h 'list)))))

(defun admit-actual (actual physical-width physical-height)
  "Admit only a full opaque RGBA octet vector of the exact physical size.
Refusal is the oracle's typed :rgba profile error carrying the actual vector."
  (unless (and (vectorp actual)
               (every (lambda (b) (typep b '(unsigned-byte 8))) actual)
               (= (length actual) (* physical-width physical-height 4))
               (loop for p from 3 below (length actual) by 4 always (= 255 (aref actual p))))
    (error 'vttest-double-size-oracle:profile-error :field :rgba :value actual)))

(defun compare-memory (snapshot actual font phase &key (scale 1))
  "One observed boolean, exact freshly hashed font vector, logical oracle raster,
then one exact whole-image comparison at the integer physical SCALE."
  (unless (member phase '(t nil)) (error "Explicit boolean phase required"))
  (let ((digest (font-digest font)))
    (unless (equal digest +font-sha+) (error "Font vector authentication refusal"))
    (let ((width (* 8 (vttest-double-size-oracle:snapshot-columns snapshot)))
          (expected (vttest-double-size-oracle:rasterize
                     snapshot font :blink-phase phase :verified-font-sha256 digest)))
      (when (member scale '(1 2))
        (admit-actual actual (* width scale) (* 384 scale)))
      (vttest-cursor-oracle:compare-physical expected actual width 384 scale))))

(defun command (root prefix argv)
  "Retain argv before execution, separate streams and the actual exit."
  (let ((base (concatenate 'string root prefix)))
    (vttest-double-size-native-contract:write-data (concatenate 'string base ".argv.sexp") argv)
    (with-open-file (out (concatenate 'string base ".stdout") :direction :output :if-exists :error)
      (with-open-file (err (concatenate 'string base ".stderr") :direction :output :if-exists :error)
        (let* ((process (sb-ext:run-program (first argv) (rest argv) :input nil
                                          :output out :error err :wait t))
               (code (sb-ext:process-exit-code process)))
          (vttest-double-size-native-contract:write-data (concatenate 'string base ".exit") code)
          (unless (eql code 0) (error "Double-size decoder failed: ~s, exit ~s" argv code)))))
    (loop for suffix in '(".argv.sexp" ".stdout" ".stderr" ".exit") collect
      (vttest-double-size-native-contract:reference root (concatenate 'string prefix suffix)))))

(defun compare-capture (root n snapshot before ready pre final native target ack surface
                        &key (scale 1))
  "Decode the original capture once, at its strict observed READY phase.
Native receipt dimensions are logical; the PNG, decoded RGBA and record are physical."
  (let* ((stem (format nil "recorder/pause-~2,'0d-" n))
         (png (concatenate 'string stem "actual.png")) (path (concatenate 'string root png))
         (phase (vttest-double-size-native-contract:required-value ready :blink-phase))
         (content (getf (vttest-double-size-oracle:snapshot-metadata snapshot) :content-blink))
         (width (* 8 (vttest-double-size-oracle:snapshot-columns snapshot))))
    (unless (= n (vttest-double-size-oracle:snapshot-pause snapshot)) (error "Foreign snapshot"))
    (vttest-double-size-native-contract:validate-frame before ready pre final native :pause n :target target
      :blink-content content :endpoint nil :ack-output ack :surface-id surface :scale scale)
    (unless (and (equal +decoder-sha+ (vttest-double-size-native-contract::digest +decoder+))
                 (= (getf native :width) width) (= (getf native :height) 384))
      (error "Native geometry/decoder refusal"))
    (let* ((physical-width (* width scale)) (physical-height (* 384 scale))
           (original (vttest-double-size-native-contract:reference root png))
           (identify (command root (concatenate 'string stem "identify")
                              (list +decoder+ "identify" "-format" "%w %h" path))))
      (with-open-file (s (concatenate 'string root stem "identify.stdout"))
        (let ((*read-eval* nil))
          (unless (and (eql physical-width (read s nil nil)) (eql physical-height (read s nil nil))
                       (eq :eof (read s nil :eof))) (error "PNG dimensions refusal"))))
      (let* ((raw (concatenate 'string stem "actual.rgba")) (raw-path (concatenate 'string root raw)))
        (when (probe-file raw-path) (error "Decoded output already exists"))
        (let* ((decode (command root (concatenate 'string stem "decode")
                                (list +decoder+ path "-depth" "8" (concatenate 'string "rgba:" raw-path))))
               (rgba (octets raw-path)) (font (octets +font+))
               (comparison (compare-memory snapshot rgba font phase :scale scale)))
          (unless (equal original (vttest-double-size-native-contract:reference root png))
            (error "PNG changed during decoding"))
          (let ((record
                  (list :version 2 :pause n :png original :phase phase :scale scale
                        :width physical-width :height physical-height
                        :font (list :path +font+ :sha256 +font-sha+)
                        :decoder (list :path +decoder+ :sha256 +decoder-sha+)
                        :bracket (loop for suffix in '("before.sexp" "ready.sexp" "pre.sexp" "final.sexp" "native.sexp")
                                       collect (vttest-double-size-native-contract:reference root (concatenate 'string stem suffix)))
                        :identify identify :decode decode :rgba (vttest-double-size-native-contract:reference root raw)
                        :decoded-length (length rgba) :opaque t :comparison comparison)))
            (vttest-double-size-native-contract:write-data (concatenate 'string root stem "comparison.sexp") record)
            (unless (getf comparison :pass) (error "Exact double-size raster mismatch at pause ~d" n))
            (vttest-double-size-native-contract:reference root (concatenate 'string stem "comparison.sexp"))))))))
