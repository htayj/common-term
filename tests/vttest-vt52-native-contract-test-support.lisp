;;;; Synthetic full-root assembly. All metadata is queued before any file is written.
(load (merge-pathnames "vttest-vt52-native-contract.lisp" *load-truename*))
(load (merge-pathnames "vttest-vt52-source-input-test-support.lisp" *load-truename*))
(load (merge-pathnames "vttest-vt52-capture-adapter-test-support.lisp" *load-truename*))
(defpackage :vttest-vt52-native-contract-tests
  (:use :cl) (:local-nicknames (:io :vttest-vt52-native-io)
    (:a :vttest-vt52-capture-adapter-tests) (:c :vttest-vt52-native-contract))
  (:export :run-tests))
(in-package :vttest-vt52-native-contract-tests)
(defvar *files*)
(defvar *root*)
(defvar *serial* 0)
(defvar *checks* 0)
(defun check (name x) (incf *checks*) (unless x (error "FAILED ~s" name)))
(defun input-bytes (events)
  (coerce (loop for e in events when (eq :wire-in (getf e :kind))
    append (coerce (getf e :bytes) 'list)) '(vector (unsigned-byte 8))))
(defun fixture-limit (path)
  ;; Original manifest and source-checksum protocol records use the production 1 MiB bound.
  (if (member path '("MANIFEST.sexp" "native/execution-inputs.sha256"
                    "native/execution-inputs-post.check") :test #'equal) 1048576 8192))
(defun queue-bytes (path bytes)
  (assert (not (assoc path *files* :test #'equal)))
  (assert (<= (length bytes) (fixture-limit path)))
  (push (cons path bytes) *files*) bytes)
(defun queue-form (path form) (queue-bytes path (a::controlled-record (copy-tree form))))
(defun queue-text (path text) (queue-bytes path (a::bytes text)))
(defun queued (path) (or (cdr (assoc path *files* :test #'equal)) (error "Not queued ~a" path)))
(defun qref (path) (list :path path :sha256 (vttest-vt52-sha256:sha256-hex (queued path))))
(defun qpath (path) (concatenate 'string *root* path))
(defun sum-row (path hash) (format nil "~a  ~a~%" hash path))
(defun qsum (path) (sum-row (qpath path) (getf (qref path) :sha256)))
(defun actual-row (path) (let ((p (namestring path))) (sum-row p (io:digest-file p))))
(defun disk-ref (path)
  (list :path path :sha256
    (io:digest-file (namestring (merge-pathnames path vttest-vt52-native-root::+source-root+)))))
(defun manifest (backend scale)
  (list :kind :vttest-area-manifest :version 1 :area "vt52-mode" :backend backend :scale scale
    :scope-status :frozen :conditions nil :navigation-pauses '(1 2 3) :required-count 1
    :native-driver (format nil "vt52-~a-v1" backend)
    :pins (mapcar #'disk-ref (vttest-vt52-source-input-tests:manifest-paths))
    :scenarios (list (list :id "vt52-01" :recorder-label "vt52" :menu-path '(7) :pauses 3
      :captures '((:id "pause-02" :pause 2)) :bytes nil :timing t
      :oracle (disk-ref "tests/vttest-vt52-oracle.lisp") :requires nil))))
(defun assemble-root (backend scale recorder-files &key mutate)
  "RECORDER-FILES contains complete original bytes, never prevalidated objects."
  (let* ((*root* (format nil "~a.build/outer333-~d-~d/" (namestring (truename "."))
                         (get-universal-time) (incf *serial*)))
         (*files* nil)
         (original (namestring (truename ".build/evidence/cursor-x11-first-screen-20261106-a/")))
         (archive (concatenate 'string original "vttest-20251205.tgz")))
    (dolist (row (funcall recorder-files *root* backend scale)) (queue-bytes (car row) (cdr row)))
    (queue-form "MANIFEST.sexp" (manifest backend scale))
    (queue-text "native/manifest-pre.sha256" (qsum "MANIFEST.sexp"))
    (queue-text "native/manifest-post.sha256" (qsum "MANIFEST.sexp"))
    (let* ((operands (vttest-vt52-source-input-tests:shell-paths))
           (paths (cons (qpath "MANIFEST.sexp") (cdr operands))))
      (assert (equal "/synthetic/MANIFEST.sexp" (first operands)))
      (queue-text "native/execution-inputs.sha256"
        (format nil "~{~a~}" (cons (qsum "MANIFEST.sexp")
          (mapcar (lambda (p) (sum-row p (getf (disk-ref p) :sha256))) (rest paths)))))
      (queue-text "native/execution-inputs-post.check" (format nil "~{~a: OK~%~}" paths)))
    (assert (and (= 243249 (length (a::read-bytes archive)))
      (equal "cd6886f9aefe6a3f6c566fa61271a55710901a71849c630bf5376aa984bf77cc" (io:digest-file archive))))
    (queue-text "build/archive.actual.sha256" (actual-row archive))
    (let ((source (source-member archive)))
      (assert (and (= 7834 (length source))
        (equal "720867fb58f57ba70cdfc12643c94d256278e325fdf43d691a440fc16ab712d4"
          (vttest-vt52-sha256:sha256-hex source))))
      (queue-bytes "build/source/vttest-20251205/vt52.c" source))
    ;; A synthetic bounded binary, never executed. The validator binds its bytes.
    (queue-text "build/source/vttest-20251205/vttest" "synthetic-not-executable")
    (queue-text "build/binary.sha256" (qsum "build/source/vttest-20251205/vttest"))
    (dolist (p '("native/binary-pre.sha256" "native/binary-post.sha256"))
      (queue-text p (qsum "build/source/vttest-20251205/vttest")))
    (queue-text "build/source.sha256" (qsum "build/source/vttest-20251205/vt52.c"))
    (queue-text "build/source-post.check" (format nil "~a: OK~%" (qpath "build/source/vttest-20251205/vt52.c")))
    (queue-text "build/tools.sha256" (actual-row vttest-vt52-capture-adapter::*decoder*))
    (queue-text "build/tools-post.check" (format nil "~a: OK~%" vttest-vt52-capture-adapter::*decoder*))
    (queue-form "build/BUILD.sexp" '(:kind :vttest-screen-build :version 1 :status :pass :reason "synthetic" :exit 0))
    (dolist (p '("native/build.exit" "build/extract.exit" "build/configure.exit" "build/make.exit"
                 "recorder/pause-02-capture.exit" "native/client.exit")) (queue-form p 0))
    (queue-form "native/stages.sexp"
      (list :kind :vttest-charsets-native-stages :version 1 :backend backend
        :entries (list (list :pause 2 :request (qref "recorder/pause-02-request")
          :invoked t :completed t :capture-exit 0 :exit-origin (qref "recorder/pause-02-capture.exit")
          :receipt (qref "recorder/pause-02-native.sexp")))))
    (queue-text "native/owned.tsv" (format nil "11 101~%12 102~%"))
    (queue-text "native/cleanup.log" "")
    (queue-form "native/owned-cleanup.sexp"
      '(:version 1 :client (:pid 11 :start-token "101" :term-exit nil :kill-exit nil :wait-exit 0 :waited t)
        :server (:pid 12 :start-token "102" :term-exit 0 :kill-exit nil :wait-exit 143 :waited t)))
    (queue-form "native/finalization-clock.sexp.start" 100)
    (queue-form "native/finalization-clock.sexp"
      (list :clock :clock-monotonic :limit-seconds 15 :start-ns 100 :end-ns 200
        :state :exited :pid 11 :token "101" :run *root* :backend backend))
    (queue-form "native/import-deadline.sexp"
      '(:clock :clock-monotonic :deadline-ns 15000000100 :validated-ns 300 :state :valid))
    (let* ((r (a::parsed (queued "recorder/RESULT.sexp")))
           (n (list :kind :vttest-vt52-run :version 3 :backend backend :scale scale
             :attempted 1 :reached 1 :captured 1 :captures 1 :navigation-pauses '(1 2 3)
             :navigation (getf r :navigation) :unverified-count 0
             :checks '((:pause 2 :pixels :pass :bytes :unverified :timing :pass))
             :status :pass :build :pass :native-started t :cleanup :pass
             :recorder-result "recorder/RESULT.sexp" :recorder-sha256 (getf (qref "recorder/RESULT.sexp") :sha256)
             :manifest (qpath "MANIFEST.sexp") :manifest-sha256 (getf (qref "MANIFEST.sexp") :sha256)
             :execution-inputs "native/execution-inputs.sha256" :evidence nil :reason "synthetic"
             :stages (list :recorder (list :origin (qref "recorder/RESULT.sexp")
                 :attempted 1 :reached 1 :captured 1 :frame-verified 1)
               :native (list :origin (qref "native/stages.sexp")
                 :request-observed 1 :capture-invoked 1 :capture-completed 1))
             :finalization (list :state :complete :client-exit 0 :result (qref "recorder/RESULT.sexp") :abort nil)
             :abort nil :unowned-endpoints nil :count-basis :native-vt52)))
      (queue-form "VT52-RUN.sexp" n))
    (when mutate (funcall mutate))
    (values *root* (reverse *files*))))
