;;;; Read-only public VT52 validation; lifecycle owns publication and CLI effects.
(load (merge-pathnames "vttest-vt52-native-root.lisp" *load-truename*))
(load (merge-pathnames "vttest-vt52-native-navigation.lisp" *load-truename*))
(load (merge-pathnames "vttest-vt52-capture-adapter.lisp" *load-truename*))
(load (merge-pathnames "vttest-vt52-native-lifecycle.lisp" *load-truename*))
(defpackage :vttest-vt52-native-contract
  (:use :cl) (:local-nicknames (:io :vttest-vt52-native-io))
  (:export :validate-vt52-native :main))
(in-package :vttest-vt52-native-contract)
(defparameter +limitations+
  '(:sequential-cursor-samples :recorder-trace-trusted :protocol-surface-ack-request-bound
    :native-ownership-harness-asserted :source-manifest-conditional :owned-capture-presentation-conditional))
(define-condition capture-refused (error) ((report :initarg :report :reader refused-report)))
(defun frame-check ()
  (list :pause 2 :pixels :pass :bytes :unverified :timing :pass :frame :pass))
(defun measure-report (report)
  (let ((*print-readably* nil) (*print-escape* t) (*print-pretty* nil) (*print-circle* nil)
        (*print-level* nil) (*print-length* nil) (*print-base* 10) (*print-radix* nil)
        (*print-case* :upcase) (*package* (find-package :cl-user)))
    (length (sb-ext:string-to-octets
      (with-output-to-string (s) (write report :stream s) (terpri s)) :external-format :utf-8))))
(defun fresh-recorder (root backend scale)
  (multiple-value-bind (r refs) (vttest-vt52-native-navigation:validate-recorder root backend scale)
    (let ((capture (vttest-vt52-capture-adapter:check-vt52-capture-directory
                    (concatenate 'string root "recorder/") :pause 2 :scale scale)))
      (io:properties capture '(:schema :verdict :reasons :pause :scale :original-hashes
        :original-hashes-after :font :decoder :png :checker :limitations))
      (unless (and (eq :vt52-capture-adapter-v1 (getf capture :schema))
        (eq :pass (getf capture :verdict)) (null (getf capture :reasons))
        (eql 2 (getf capture :pause)) (eql scale (getf capture :scale)))
        (error 'capture-refused :report capture))
      (let* ((c (first (getf r :checks)))
             (native (io:read-data root "recorder/pause-02-native.sexp")))
        (io:need (equal native (getf c :native)) "Recorder native identity mismatch")
        (loop for (k suffix) on '(:before "before.sexp" :ready "ready.sexp" :request "request"
              :request-sexp "request.sexp" :native-ready "native-ready.sexp" :pre "pre.sexp"
              :final "final.sexp" :native "native.sexp" :control "control.sexp") by #'cddr do
          (io:need (equal (io:digest-file (io:local-path root (concatenate 'string "recorder/pause-02-" suffix)))
                         (getf (getf capture :original-hashes) k)) "Capture original changed")))
      (dolist (ref refs) (io:check-reference ref root))
      (values r capture refs))))
(defun validate-recorder-import (run-dir)
  (let* ((root (io:canonical-root run-dir)) (m (io:read-data root "MANIFEST.sexp" :protocol t)))
    (io:properties m '(:kind :version :area :backend :scale :scope-status :conditions :scenarios
                      :required-count :native-driver :pins :navigation-pauses))
    (io:need (equal '(1 2 3) (getf m :navigation-pauses)))
    (vttest-vt52-native-root:validate-manifest m (getf m :backend) (getf m :scale))
    (multiple-value-bind (r capture refs) (fresh-recorder root (getf m :backend) (getf m :scale))
      (declare (ignore capture refs))
      (let ((origin (io:reference root "recorder/RESULT.sexp")))
        (list :origin origin :status :pass
          :stage (list :origin (copy-list origin) :attempted (getf r :attempted)
            :reached (getf r :reached) :captured (getf r :captured) :frame-verified (getf r :frame-verified))
          :checks (list (list :pause 2 :pixels :pass :bytes :unverified :timing :pass)))))))
(defun validate-evidence (run-dir &key candidate backend scale)
  (let ((component nil) (capture nil) (record nil) (checks nil) (refs nil)
        (reason :validation-error) (frames 0) (root nil) (passed nil))
    (labels ((report ()
      (list :schema :vt52-native-check-v1 :kind :vttest-vt52-native-check :version 1
        :verdict (if passed :pass :refuse) :reasons (unless passed (list reason))
        :backend backend :scale scale :pause 2 :pauses '(2) :navigation-pauses '(1 2 3)
        :valid-frames frames
        :counts (list :required 1 :attempted (and record 1) :reached (and record 1)
          :captured (and record 1) :verified (and (= frames 1) 1) :passed (and passed 1)
          :failed (and passed 0) :unverified (and passed 0))
        :checks checks :capture capture :ledger (getf component :ledger)
        :provenance (getf component :provenance) :cleanup (getf component :cleanup)
        :limitations +limitations+)))
      (handler-case
        (progn
          (let ((value (vttest-vt52-native-root:validate-root run-dir
                         :backend backend :scale scale :candidate candidate)))
            (io:properties value '(:verdict :reasons :mode :root :backend :scale :manifest
              :native-run :ledger :source-root :sources :originals :provenance :cleanup))
            (setf component value))
          (unless (eq :pass (getf component :verdict))
            (let ((why (getf component :reasons)))
              (when (and (consp why) (keywordp (first why))) (setf reason (first why))))
            (error "Root refusal"))
          (io:need (eq (if candidate :candidate :final) (getf component :mode)))
          (setf root (getf component :root) backend (getf component :backend) scale (getf component :scale)
                reason :invalid-navigation)
          (multiple-value-setq (record capture refs) (fresh-recorder root backend scale))
          (setf frames 1 checks (frame-check) reason :invalid-recorder)
          (let ((n (getf component :native-run)))
            (io:need (and (equal (getf n :navigation) (getf record :navigation))
              (equal (getf n :recorder-sha256) (getf (io:reference root "recorder/RESULT.sexp") :sha256)))))
          (setf reason :invalid-ledger)
          (let* ((l (getf component :ledger)) (c (first (getf record :checks))))
            (io:need l)
            (io:need (equal (getf c :native) (io:read-data root "recorder/pause-02-native.sexp"))))
          (setf reason :evidence-changed)
          (dolist (ref (append refs (getf component :originals))) (io:check-reference ref root))
          (loop for (k ref) on (getf component :provenance) by #'cddr
            unless (member k '(:root :pins)) do (when ref (io:check-reference ref root)))
          (loop for p in (cdr (getf (getf component :provenance) :pins)) by #'cddr do
            (io:need (equal (getf p :sha256) (io:digest-file (getf p :path)))))
          (dolist (row (getf component :sources))
            (io:need (and (= 2 (length row)) (stringp (first row)) (io:hex-p (second row))))
            (let ((p (first row)))
              (io:need (equal (second row) (io:digest-file
                (if (char= #\/ (char p 0)) p
                  (io:local-path (getf component :source-root)
                    (if (search "./" p :end2 2) (subseq p 2) p))))))))
          (setf passed t reason nil))
        (capture-refused (c) (setf passed nil reason :capture-refused capture (refused-report c)))
        (error () (setf passed nil)))
      (let ((result (report)))
        (if (<= (measure-report result) 8192) result
          (progn (setf passed nil reason :receipt-too-large capture nil component nil checks nil)
                 (report)))))))
(defun validate-vt52-native (run-dir &key backend scale)
  (unless (and (or (null backend) (member backend '("x11" "wayland") :test #'equal))
               (or (null scale) (member scale '(1 2))))
    (setf run-dir "" backend nil scale nil))
  (validate-evidence run-dir :backend backend :scale scale))
(defun main ()
  (vttest-vt52-native-lifecycle:main :validate-function #'validate-evidence
                                  :import-function #'validate-recorder-import))
