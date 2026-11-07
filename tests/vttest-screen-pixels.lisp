;;;; Exact screen-only comparison of the current owned capture.
(defpackage #:vttest-screen-pixels
  (:use #:cl)
  (:export #:compare-capture))
(in-package #:vttest-screen-pixels)

(defparameter +font+
  "/nix/store/9qvd27vg12177prxcl778rdhxh4lk2mi-terminus-font-4.49.1/share/fonts/terminus/ter-u16n.otb")
(defparameter +font-sha+ "0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca")
(defparameter +decoder+
  "/nix/store/h74lg8wfwndnf5glr171kmcshlffi7l2-imagemagick-7.1.2-29/bin/magick")
(defparameter +decoder-sha+ "054c52b583c303bca2aae9fadcfd9e09fbc946f83e6621b39f22650d28d3027e")

(defun octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (unless (= (length bytes) (read-sequence bytes s)) (error "Short image/font read"))
      bytes)))

(defun command (root prefix argv)
  "Retain exact argv before execution, separate streams and the actual exit."
  (let ((base (concatenate 'string root prefix)))
    (vttest-screen-native-contract:write-data (concatenate 'string base ".argv.sexp") argv)
    (with-open-file (out (concatenate 'string base ".stdout") :direction :output :if-exists :error)
      (with-open-file (err (concatenate 'string base ".stderr") :direction :output :if-exists :error)
        (let* ((process (sb-ext:run-program (first argv) (rest argv) :input nil
                                          :output out :error err :wait t))
               (code (sb-ext:process-exit-code process)))
          (vttest-screen-native-contract:write-data (concatenate 'string base ".exit") code)
          (unless (eql code 0) (error "Screen decoder failed: ~s, exit ~s" argv code)))))
    (loop for suffix in '(".argv.sexp" ".stdout" ".stderr" ".exit") collect
      (vttest-screen-native-contract:reference root (concatenate 'string prefix suffix)))))

(defun compare-capture (root n snapshot before ready pre final native target ack surface)
  "Admit the strict current bracket before decoding once at the observed phase."
  (let* ((stem (format nil "recorder/pause-~2,'0d-" n))
         (png (concatenate 'string stem "actual.png"))
         (path (concatenate 'string root png))
         (phase (vttest-screen-frame:required-value ready :blink-phase))
         (content (getf (vttest-screen-oracle:snapshot-metadata snapshot) :blink-visible))
         (width (* 8 (vttest-screen-oracle:snapshot-columns snapshot))))
    (vttest-screen-frame:validate-frame before ready pre final native :pause n :target target
      :blink-content content :endpoint nil :ack-output ack :surface-id surface)
    (unless (and (= (getf native :width) width) (= (getf native :height) 384)
                 (equal (getf native :png) (format nil "pause-~2,'0d-actual.png" n))
                 (eq t (getf native :owned)) (eql 0 (getf native :capture-exit))
                 (eql 1 (getf native :capture-count)))
      (error "Current PNG/native identity refusal"))
    (unless (and (equal +font-sha+ (vttest-screen-native-contract::digest +font+))
                 (equal +decoder-sha+ (vttest-screen-native-contract::digest +decoder+)))
      (error "Screen reference font/decoder pin refusal"))
    (let* ((original (vttest-screen-native-contract:reference root png))
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
               (comparison (vttest-screen-oracle:compare-screen-snapshot
                            snapshot rgba (octets +font+) :blink-phase phase)))
          (unless (equal original (vttest-screen-native-contract:reference root png))
            (error "PNG changed during decoding"))
          (let ((record
                  (list :version 1 :pause n :png original :phase phase :width width :height 384
                        :font (list :path +font+ :sha256 +font-sha+)
                        :decoder (list :path +decoder+ :sha256 +decoder-sha+)
                        :bracket (loop for suffix in '("before.sexp" "ready.sexp" "pre.sexp" "final.sexp" "native.sexp")
                                       collect (vttest-screen-native-contract:reference root (concatenate 'string stem suffix)))
                        :identify identify :decode decode
                        :rgba (vttest-screen-native-contract:reference root raw)
                        :decoded-length (length rgba) :opaque t :comparison comparison)))
            (vttest-screen-native-contract:write-data (concatenate 'string root stem "comparison.sexp") record)
            (unless (getf comparison :pass) (error "Exact screen raster mismatch at pause ~d" n))
            (vttest-screen-native-contract:reference root (concatenate 'string stem "comparison.sexp"))))))))
