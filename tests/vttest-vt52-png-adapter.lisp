;;;; Original PNG decode boundary. No application dependencies.
(load (merge-pathnames "vttest-vt52-sha256.lisp" *load-truename*))
(defpackage #:vttest-vt52-png-adapter
  (:use #:cl) (:export #:decode-original-png #:png-refusal #:refusal-reason
                      #:refusal-evidence))
(in-package #:vttest-vt52-png-adapter)
(defparameter *decoder*
  "/nix/store/h74lg8wfwndnf5glr171kmcshlffi7l2-imagemagick-7.1.2-29/bin/magick")
(defparameter *decoder-hash*
  "054c52b583c303bca2aae9fadcfd9e09fbc946f83e6621b39f22650d28d3027e")
(define-condition png-refusal (error)
  ((reason :initarg :reason :reader refusal-reason)
   (evidence :initarg :evidence :reader refusal-evidence))
  (:report (lambda (c s) (format s "PNG refusal: ~a" (refusal-reason c)))))
(defun refuse (reason &optional evidence)
  (error 'png-refusal :reason reason :evidence evidence))
(defun octets (x) (typep x '(vector (unsigned-byte 8))))
(defun read-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (unless (= (length v) (read-sequence v s)) (refuse :short-read)) v)))
(defun digest (path)
  (vttest-vt52-sha256:sha256-hex (read-bytes path)))
(defun owned-file ()
  ;; OPEN with :IF-EXISTS NIL uses exclusive creation, never truncation.
  (loop repeat 100 for path = (format nil "/tmp/vt52-png-~36r-~36r"
                                      (get-universal-time) (random (expt 2 64)))
        for s = (open path :direction :output :element-type '(unsigned-byte 8)
                      :if-exists nil :if-does-not-exist :create)
        when s do (close s) (return path)
        finally (refuse :temporary-file)))
(defun run-decoder (argv)
  ;; Streams remain separate and binary, including on a nonzero exit.
  (unless (string= (digest *decoder*) *decoder-hash*) (refuse :decoder-pin))
  (let ((out (owned-file)) (err (owned-file)))
    (unwind-protect
         (with-open-file (o out :direction :output :element-type '(unsigned-byte 8)
                                 :if-exists :overwrite)
           (with-open-file (e err :direction :output :element-type '(unsigned-byte 8)
                                   :if-exists :overwrite)
             (let ((p (sb-ext:run-program (first argv) (rest argv) :search nil
                          :input nil :output o :error e :wait t)))
               (finish-output o) (finish-output e)
               (values (read-bytes out) (read-bytes err)
                       (sb-ext:process-exit-code p)))))
      (delete-file out) (delete-file err))))
(defun command-receipt (argv runner)
  (multiple-value-bind (out err exit) (funcall runner (copy-list argv))
    (unless (and (octets out) (octets err) (integerp exit) (<= 0 exit 255))
      (refuse :malformed-process-output))
    (let ((r (list :argv argv :stdout out :stderr err :exit exit)))
      (unless (zerop exit) (refuse :decoder-failed r)) r)))
(defun decode-original-png (path &key scale (decoder-argv (list *decoder*))
                                         (runner #'run-decoder))
  "Return a decode plist or signal PNG-REFUSAL (reason/evidence accessors).
DECODER-ARGV is the closed executable prefix, exactly (pinned-magick).
RUNNER receives each complete argv and returns stdout octets, stderr octets,
numeric exit. For decode it must write the argv's rgba: destination file.
The two calls use the pinned identify and file-output decode procedures.
Success :ARGV/:EXIT describe decode; :IDENTIFY holds the other receipt.
Both receipts retain complete separate :STDOUT/:STDERR octets. Injection
is for synthetic tests, not evidence of executing the pinned decoder."
  (handler-case
      (progn
        (unless (member scale '(1 2)) (refuse :scale))
        (unless (equal decoder-argv (list *decoder*)) (refuse :decoder-argv))
        (let* ((original (namestring (truename path)))
               (before (digest original)) (rgba-path (owned-file)))
          (unwind-protect
               (let* ((identify (command-receipt
                                (list *decoder* "identify" "-format" "%w %h" original)
                                runner))
                      (width (* 640 scale)) (height (* 384 scale)))
                 (unless (equalp (getf identify :stdout)
                                 (map '(vector (unsigned-byte 8)) #'char-code
                                      (format nil "~d ~d" width height)))
                   (refuse :dimensions identify))
                 (let* ((decoded (command-receipt
                                  (list *decoder* original "-depth" "8"
                                        (concatenate 'string "rgba:" rgba-path)) runner))
                        (rgba (read-bytes rgba-path)) (after (digest original)))
                   (unless (string= before after)
                     (refuse :changed-original (list :sha256-before before
                         :sha256-after after :identify identify :decode decoded)))
                   (unless (= (length rgba) (* width height 4))
                     (refuse :rgba-length decoded))
                   (append (list :sha256-before before :sha256-after after
                                 :width width :height height :rgba rgba
                                 :identify identify) decoded)))
            (delete-file rgba-path))))
    (png-refusal (c) (error c))
    (error () (refuse :io-or-runner-error))))
