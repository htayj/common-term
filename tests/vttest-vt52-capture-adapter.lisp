;;;; Original-file VT52 admission and strict full-frame comparison boundary.
(load (merge-pathnames "vttest-vt52-native-check.lisp" *load-truename*))
(load (merge-pathnames "vttest-vt52-png-adapter.lisp" *load-truename*))
(require :sb-posix)
(defpackage #:vttest-vt52-capture-adapter
  (:use #:cl)
  (:local-nicknames (#:a #:vttest-vt52-admission)
                    (#:h #:vttest-vt52-sha256)
                    (#:p #:vttest-vt52-png-adapter))
  (:export #:check-vt52-capture-directory))
(in-package #:vttest-vt52-capture-adapter)

(defparameter *font-path*
  #P"/nix/store/9qvd27vg12177prxcl778rdhxh4lk2mi-terminus-font-4.49.1/share/fonts/terminus/ter-u16n.otb")
(defparameter *font-pin*
  "0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca")
(defparameter *decoder*
  "/nix/store/h74lg8wfwndnf5glr171kmcshlffi7l2-imagemagick-7.1.2-29/bin/magick")
(defparameter *decoder-pin*
  "054c52b583c303bca2aae9fadcfd9e09fbc946f83e6621b39f22650d28d3027e")
(defparameter *inputs*
  '(:before "before.sexp" :ready "ready.sexp" :request "request"
    :request-sexp "request.sexp" :native-ready "native-ready.sexp"
    :pre "pre.sexp" :final "final.sexp" :native "native.sexp"
    :control "control.sexp"))
(defparameter *limitations*
  '(:sequential-cursor-samples :recorder-trace-trusted
    :protocol-surface-ack-request-bound :native-ownership-harness-asserted
    :source-manifest-conditional :owned-capture-presentation-conditional))

(define-condition capture-refusal (error)
  ((reason :initarg :reason :reader reason)))
(defun refuse (reason) (error 'capture-refusal :reason reason))
(defun read-original (path limit name)
  "Read bounded original octets, never parse or reserialize a receipt here."
  (handler-case
      (progn
        (unless (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:lstat path)))
          (refuse (list :nonregular name)))
        (with-open-file (s path :element-type '(unsigned-byte 8))
          (let ((n (file-length s)))
            (unless (<= n limit) (refuse (list :too-large name)))
            (let ((bytes (make-array n :element-type '(unsigned-byte 8))))
              (unless (and (= n (read-sequence bytes s)) (null (read-byte s nil nil)))
                (refuse (list :changed-length name)))
              bytes))))
    (capture-refusal (c) (error c))
    (file-error () (refuse (list :missing-or-unreadable name)))
    (error () (refuse (list :input-io name)))))

(defun summarize (value)
  "Keep binary process streams separate, but persist only their hashes and sizes."
  (cond ((typep value '(vector (unsigned-byte 8)))
         (list :bytes (length value) :sha256 (h:sha256-hex value)))
        ((consp value) (cons (summarize (car value)) (summarize (cdr value))))
        (t value)))

(defun check-vt52-capture-directory (dir &key pause scale)
  "Read nine original pause-02 files, admit, decode the original PNG, then check.
DIR is the recorder directory, resolved by TRUENAME; source modules load relative
 to this file in the prepared tree. The immutable Nix font path is the reviewed
 charsets-contract pin. No legacy rasterizer, decoder injection or capture retry.
The report is conditional evidence, not root/manifest/ledger acceptance."
  (let (hashes hashes-after font-receipt decoded checker reasons)
    (labels ((report ()
               ;; Copy only this trusted assembled report, never untrusted inputs.
               (copy-tree
                (list :schema :vt52-capture-adapter-v1
                     :verdict (if reasons :refuse :pass) :reasons reasons
                     :pause (and (eql pause 2) 2) :scale (and (member scale '(1 2)) scale)
                     :original-hashes hashes
                     :original-hashes-after hashes-after :font font-receipt
                     :decoder (list :path *decoder* :sha256 *decoder-pin*
                                    :authentication :real-runner-per-command)
                     :png decoded :checker checker :limitations *limitations*))))
      (handler-case
          (progn
            (unless (eql pause 2) (refuse :pause))
            (unless (member scale '(1 2)) (refuse :scale))
            (unless (typep dir '(or string pathname)) (refuse :directory))
            (let* ((root (truename (uiop:ensure-directory-pathname dir)))
                   (raw nil))
              (flet ((input-path (suffix)
                       (merge-pathnames (concatenate 'string "pause-02-" suffix) root)))
                (loop for (key suffix) on *inputs* by #'cddr
                      for bytes = (read-original (input-path suffix) 8192 key)
                      do (setf raw (append raw (list key bytes))
                               hashes (append hashes (list key (h:sha256-hex bytes)))))
                (multiple-value-bind (admission refused)
                    (apply #'a:admit-vt52-capture :pause pause raw)
                  (unless (and admission (a:admission-p admission) (null refused))
                    (setf reasons (or refused '(:admission-refused)))
                    (return-from check-vt52-capture-directory (report)))
                  ;; Only admitted originals reach font parsing or the real decoder.
                  (let* ((font (read-original *font-path* 38848 :font))
                         (font-hash (h:sha256-hex font)))
                    (setf font-receipt (list :path (namestring *font-path*)
                                             :bytes (length font) :sha256 font-hash))
                    (unless (and (= 38848 (length font)) (string= *font-pin* font-hash))
                      (refuse :font-pin))
                    (let ((png (p:decode-original-png (input-path "actual.png")
                                 :scale scale :decoder-argv (list *decoder*))))
                      (setf decoded (summarize png))
                      (setf checker
                            (vttest-vt52-native-check:validate-vt52-capture
                             :admission admission
                             :snapshot (vttest-vt52-oracle:vt52-snapshot)
                             :font font :font-sha256 font-hash :scale scale
                             :rgba (getf png :rgba) :width (getf png :width)
                             :height (getf png :height))))
                    ;; Bind the admitted originals across the potentially slow decode.
                    (loop for (key suffix) on *inputs* by #'cddr
                          for bytes = (read-original (input-path suffix) 8192 key)
                          do (setf hashes-after
                                   (append hashes-after (list key (h:sha256-hex bytes)))))
                    (unless (equal hashes hashes-after) (refuse :changed-originals))
                    (unless (and (eq :pass (getf checker :verdict))
                                 (null (getf checker :reasons)))
                      (setf reasons (or (getf checker :reasons) '(:checker-refused)))))))))
        (capture-refusal (c) (setf reasons (list (reason c))))
        (p:png-refusal (c)
          (setf decoded (summarize (p:refusal-evidence c))
                reasons (list (list :png (p:refusal-reason c)))))
        (error () (setf reasons '(:adapter-io-or-checker-error))))
      (report))))
