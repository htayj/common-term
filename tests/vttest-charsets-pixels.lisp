;;;; Exact character-set comparison of the current owned capture.
(defpackage #:vttest-charsets-pixels
  (:use #:cl)
  (:export #:compare-capture))
(in-package #:vttest-charsets-pixels)

(defparameter +font+
  "/nix/store/9qvd27vg12177prxcl778rdhxh4lk2mi-terminus-font-4.49.1/share/fonts/terminus/ter-u16n.otb")
(defparameter +font-sha+ "0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca")
(defparameter +decoder+
  "/nix/store/h74lg8wfwndnf5glr171kmcshlffi7l2-imagemagick-7.1.2-29/bin/magick")
(defparameter +decoder-sha+ "054c52b583c303bca2aae9fadcfd9e09fbc946f83e6621b39f22650d28d3027e")

;;; Pure byte hashing closure from vttest-area, no dispatcher load.
(defparameter *sha-k*
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
    #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
    #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))
(defun u32 (n) (logand n #xffffffff))
(defun rotr (n k) (u32 (logior (ash n (- k)) (ash n (- 32 k)))))
(defun sha256 (bytes)
  (let* ((n (length bytes)) (size (* 64 (ceiling (+ n 9) 64)))
         (data (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
         (h (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
         (w (make-array 64 :initial-element 0)))
    (replace data bytes) (setf (aref data n) 128)
    (dotimes (i 8) (setf (aref data (- size 1 i)) (ldb (byte 8 (* i 8)) (* n 8))))
    (loop for offset from 0 below size by 64 do
      (dotimes (i 16)
        (setf (aref w i) (loop for j below 4 sum (ash (aref data (+ offset (* i 4) j)) (* 8 (- 3 j))))))
      (loop for i from 16 below 64 do
        (let ((x (aref w (- i 15))) (y (aref w (- i 2))))
          (setf (aref w i) (u32 (+ (aref w (- i 16)) (logxor (rotr x 7) (rotr x 18) (ash x -3))
                                        (aref w (- i 7)) (logxor (rotr y 17) (rotr y 19) (ash y -10)))))))
      (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
            (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
        (dotimes (i 64)
          (let* ((t1 (u32 (+ hh (logxor (rotr e 6) (rotr e 11) (rotr e 25))
                            (logxor (logand e f) (logand (lognot e) g)) (aref *sha-k* i) (aref w i))))
                 (t2 (u32 (+ (logxor (rotr a 2) (rotr a 13) (rotr a 22))
                            (logxor (logand a b) (logand a c) (logand b c))))))
            (psetf hh g g f f e e (u32 (+ d t1)) d c c b b a a (u32 (+ t1 t2)))))
        (loop for x in (list a b c d e f g hh) for i from 0 do (setf (aref h i) (u32 (+ (aref h i) x))))))
    (string-downcase (format nil "~{~8,'0X~}" (coerce h 'list)))))

(defun octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (unless (= (length bytes) (read-sequence bytes s)) (error "Short image/font read"))
      bytes)))

(defun command (root prefix argv)
  "Retain exact argv before execution, separate streams and the actual exit."
  (let ((base (concatenate 'string root prefix)))
    (vttest-charsets-native-contract:write-data (concatenate 'string base ".argv.sexp") argv)
    (with-open-file (out (concatenate 'string base ".stdout") :direction :output :if-exists :error)
      (with-open-file (err (concatenate 'string base ".stderr") :direction :output :if-exists :error)
        (let* ((process (sb-ext:run-program (first argv) (rest argv) :input nil
                                          :output out :error err :wait t))
               (code (sb-ext:process-exit-code process)))
          (vttest-charsets-native-contract:write-data (concatenate 'string base ".exit") code)
          (unless (eql code 0) (error "Character-set decoder failed: ~s, exit ~s" argv code)))))
    (loop for suffix in '(".argv.sexp" ".stdout" ".stderr" ".exit") collect
      (vttest-charsets-native-contract:reference root (concatenate 'string prefix suffix)))))

(defun validate-comparison-facts (comparison)
  "Validate the actual public comparator result without absent-field defaults."
  (vttest-charsets-native-contract::tree comparison)
  (vttest-charsets-native-contract::properties comparison
    '(:pass :compared-pixels :different-pixels :different-cells :first-difference :tolerance :masked-pixels))
  (unless (and (member (getf comparison :pass) '(t nil))
               (eql 245760 (getf comparison :compared-pixels))
               (typep (getf comparison :different-pixels) '(integer 0 245760))
               (listp (getf comparison :different-cells))
               (listp (getf comparison :first-difference))
               (eql 0 (getf comparison :tolerance)) (eql 0 (getf comparison :masked-pixels)))
    (error "Malformed full comparison facts"))
  (if (getf comparison :pass)
      (unless (and (eql 0 (getf comparison :different-pixels))
                   (null (getf comparison :different-cells)) (null (getf comparison :first-difference)))
        (error "Contradictory comparison PASS"))
      (unless (and (plusp (getf comparison :different-pixels))
                   (getf comparison :different-cells) (getf comparison :first-difference))
        (error "Contradictory comparison failure")))
  comparison)

(defun compare-capture (root n snapshot before ready pre final native target ack surface)
  "Admit the strict current bracket before decoding once at the observed phase."
  (unless (eql n 1) (error "Character-set pause must be 1"))
  (vttest-charsets-oracle:require-complete snapshot)
  (let* ((stem (format nil "recorder/pause-~2,'0d-" n))
         (png (concatenate 'string stem "actual.png"))
         (path (concatenate 'string root png))
         (phase (vttest-screen-frame:required-value ready :blink-phase))
         (content nil)
         (width 640))
    (vttest-screen-frame:validate-frame before ready pre final native :pause n :target target
      :blink-content content :endpoint nil :ack-output ack :surface-id surface)
    (unless (and (= (getf native :width) width) (= (getf native :height) 384)
                 (equal (getf native :png) (format nil "pause-~2,'0d-actual.png" n))
                 (eq t (getf native :owned)) (eql 0 (getf native :capture-exit))
                 (eql 1 (getf native :capture-count)))
      (error "Current PNG/native identity refusal"))
    (unless (and (equal +font-sha+ (vttest-charsets-native-contract::digest +font+))
                 (equal +decoder-sha+ (vttest-charsets-native-contract::digest +decoder+)))
      (error "Character-set reference font/decoder pin refusal"))
    (let* ((original (vttest-charsets-native-contract:reference root png))
           (identify (command root (concatenate 'string stem "identify")
                              (list +decoder+ "identify" "-format" "%w %h" path))))
      (with-open-file (s (concatenate 'string root stem "identify.stdout"))
        (let ((*read-eval* nil))
          (unless (and (eql width (read s nil nil)) (eql 384 (read s nil nil))
                       (eq :eof (read s nil :eof))) (error "PNG dimensions refusal"))))
      (let* ((raw (concatenate 'string stem "actual.rgba"))
             (raw-path (concatenate 'string root raw)))
        (when (probe-file raw-path) (error "Decoded output already exists"))
        (let* ((decode (command root (concatenate 'string stem "decode")
                                (list +decoder+ path "-depth" "8" (concatenate 'string "rgba:" raw-path))))
               (rgba (octets raw-path))
               (font (octets +font+))
               (font-sha (sha256 font))
               (comparison
                 (progn
                   (unless (equal +font-sha+ font-sha) (error "Actual font vector digest refusal"))
                   (unless (and (= 983040 (length rgba))
                                (loop for i from 3 below (length rgba) by 4 always (= 255 (aref rgba i))))
                     (error "Decoded full opaque RGBA refusal"))
                   (vttest-charsets-oracle:compare-snapshot
                     snapshot rgba font :font-sha256 font-sha :blink-phase phase))))
          (validate-comparison-facts comparison)
          (unless (and (equal +font-sha+ (vttest-charsets-native-contract::digest +font+))
                       (equal +decoder-sha+ (vttest-charsets-native-contract::digest +decoder+)))
            (error "Font/decoder changed during comparison"))
          (unless (equal original (vttest-charsets-native-contract:reference root png))
            (error "PNG changed during decoding"))
          (let ((record
                  (list :version 1 :pause n :png original :phase phase :width width :height 384
                        :font (list :path +font+ :sha256 +font-sha+)
                        :decoder (list :path +decoder+ :sha256 +decoder-sha+)
                        :bracket (loop for suffix in '("before.sexp" "ready.sexp" "pre.sexp" "final.sexp" "native.sexp")
                                       collect (vttest-charsets-native-contract:reference root (concatenate 'string stem suffix)))
                        :identify identify :decode decode
                        :rgba (vttest-charsets-native-contract:reference root raw)
                        :decoded-length (length rgba) :opaque t :comparison comparison)))
            (vttest-charsets-native-contract:write-data (concatenate 'string root stem "comparison.sexp") record)
            (unless (getf comparison :pass) (error "Exact character-set raster mismatch at pause ~d" n))
            (vttest-charsets-native-contract:reference root (concatenate 'string stem "comparison.sexp"))))))))
