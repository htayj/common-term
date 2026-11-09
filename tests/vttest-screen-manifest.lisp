;;;; Executable screen manifest and native dispatch helpers.
(in-package :vttest-screen-driver)
;;;; Registration complements the independently scoped offline interval API.
(export '(validate run make-manifest summarize-native invoke-native))
(load (merge-pathnames "vttest-screen-native-contract.lisp" *load-truename*))
(defparameter +screen-archive+
  ".build/evidence/cursor-x11-first-screen-20261106-a/vttest-20251205.tgz")
(defparameter +screen-source-sha+
  "cd6886f9aefe6a3f6c566fa61271a55710901a71849c630bf5376aa984bf77cc")
(defparameter +screen-bash+
  "/nix/store/kwyi4bq5s2qgwbhz6cs4gjy57c2zdxws-bash-interactive-5.3p20/bin/bash")
(defparameter +screen-pins+
  '("tests/vttest-area.lisp" "tests/vttest-screen-driver.lisp"
    "tests/vttest-screen-build.sh" "tests/vttest-screen-native.sh"
    "tests/vttest-screen-recorder.lisp" "tests/vttest-screen-frame.lisp"
    "tests/vttest-screen-manifest.lisp"
    "tests/vttest-screen-native-contract.lisp"
    "tests/vttest-screen-pixels.lisp" "tests/vttest-cursor-oracle.lisp"
    "tests/vttest-screen-oracle.lisp" "tests/vttest-screen-wire.lisp"
    "tests/vttest-selection.lisp" "tests/verify.sh"
    "src/host.lisp" "src/window.lisp"))
(defun screen-digest (path)
  (vttest-area::sha256 (vttest-cursor-driver::bytes path)))
(defun screen-scenarios ()
  (loop for pause from 1 to 15 collect
    (list :id (format nil "screen-~2,'0d" pause) :recorder-label "screen"
          :menu-path '(2) :pauses 15
          :captures (list (list :id (format nil "pause-~2,'0d" pause) :pause pause))
          :bytes nil :timing t
          :oracle (list :path "tests/vttest-screen-oracle.lisp"
                        :sha256 (screen-digest "tests/vttest-screen-oracle.lisp"))
          :requires nil)))
(defun make-manifest (backend &optional (scale 1))
  "Fresh executable declaration, not a claim that any native check was observed."
  (vttest-area::need (member backend '("x11" "wayland") :test #'equal) :unknown-backend)
  (vttest-area::check-scale backend scale)
  (list :kind :vttest-area-manifest :version 1 :area "screen" :backend backend :scale scale
        :scope-status :frozen :conditions '("private-headless" "source-built-per-run"
          "all-15-fixed-holds" "strict-original-frame-bracket" "pause14-blink-on"
          "content-aware-blink" "fresh-native-custody")
        :scenarios (screen-scenarios) :required-count 15
        :native-driver (format nil "screen-~a-v1" backend)
        :pins (cons (list :path +screen-archive+ :sha256 +screen-source-sha+)
                    (loop for path in +screen-pins+
                          collect (list :path path :sha256 (screen-digest path))))))
(defun validate (manifest)
  "Closed visible-screen contract with independent exact reference comparison."

  (vttest-area::need (equal (screen-digest +screen-archive+) +screen-source-sha+)
                    :screen-source-archive-mismatch)
  (let ((expected (make-manifest (getf manifest :backend) (getf manifest :scale))))
    (vttest-area::need (equal manifest expected) :screen-manifest-mismatch))
  manifest)
(defun summarize-native (manifest record process-exit &optional root)
  "Validate identities and all closed stage contracts before retaining counts."
  (let ((counts nil) (checks nil) (receipt-error nil) (identity nil))
    (handler-case
        (when record
          (vttest-screen-native-contract:validate-native record (getf manifest :backend) root)
          (setf counts (append (loop for key in '(:attempted :reached :captured)
                                     append (list key (getf record key)))
                               (list :stages (copy-tree (getf record :stages))))
                checks (getf record :checks) identity t))
      (error (e) (setf receipt-error (princ-to-string e))))
    (unless identity (setf record nil))
    (let ((observations nil) (passed 0) (failed 0) (unverified 0) (verified 0)
          (process-failed (and process-exit (not (member process-exit '(0 3))))))
    (loop for scenario in (getf manifest :scenarios) for pause from 1 do
      (let* ((check (find pause checks :key (lambda (c) (getf c :pause))))
             (pixels (or (getf check :pixels) :unverified))
             (bytes (or (getf check :bytes) :unverified))
             (timing (or (getf check :timing) :unverified))
             (states (list pixels timing)))
        (unless (member :unverified states) (incf verified))
        (cond ((member :fail states) (incf failed))
              ((member :unverified states) (incf unverified))
              (t (incf passed)))
        (push (list :scenario (getf scenario :id)
                    :capture (getf (first (getf scenario :captures)) :id)
                    :fresh (and check (eql process-exit 0) (eq (getf record :status) :pass)
                                (every (lambda (key) (eql (getf record key) 15))
                                       '(:attempted :reached :captured)))
                    :fixture nil :pixels pixels :bytes bytes :timing timing) observations)))
    ;; A successful-looking subset cannot turn a partial native run into PASS.
    (when (or receipt-error (not identity) (not (eql process-exit 0))
              (not (eq (getf record :status) :pass)))
      (dolist (o observations) (setf (getf o :fresh) nil)))
    (values (nreverse observations)
            (list :native-exit process-exit :native-record record
                  :receipt-error receipt-error
                  :native-failed (or process-failed receipt-error
                                     (and identity (or (eq (getf record :status) :fail)
                                                       (getf record :abort)
                                                       (member (getf (getf record :finalization) :state)
                                                               '(:timeout :invalid-result :exited-without-result)))))
                  :counts (append (list :required 15) counts
                                  (unless counts (list :attempted nil :reached nil :captured nil :stages nil))
                                  (list :verified verified :passed passed :failed failed :unverified unverified)))))))
(defun invoke-native (backend root manifest-path)
  "One launch, no retries. Retain streams and actual process exit."
  (let ((script (merge-pathnames "tests/vttest-screen-native.sh" (truename "."))))
    (unless (probe-file script) (return-from invoke-native nil))
    (vttest-cursor-driver::put (concatenate 'string root "NATIVE-COMMAND.sexp")
      (list :program +screen-bash+ :arguments (list (namestring script) backend root manifest-path)
            :input :null :stdout "native.stdout" :stderr "native.stderr"))
    (let ((process (sb-ext:run-program +screen-bash+
                     (list (namestring script) backend root manifest-path)
                     :search nil :input nil
                     :output (concatenate 'string root "native.stdout")
                     :error (concatenate 'string root "native.stderr")
                     :if-output-exists :error :if-error-exists :error :wait t)))
      (let ((exit (sb-ext:process-exit-code process)))
        (vttest-cursor-driver::put (concatenate 'string root "NATIVE-EXIT.sexp")
                                  (list :exit exit :status (sb-ext:process-status process)))
        exit))))
(defun run (manifest directory manifest-path manifest-hash head tree)
  (declare (ignore head tree))
  (validate manifest)
  (let* ((root (namestring (truename directory)))
         (path (namestring (truename manifest-path)))
         (manifest-binding (list :manifest path :manifest-sha256 (screen-digest path)
                                 :source-pins (copy-tree (getf manifest :pins))))
         (binding-check (vttest-area::need
                         (equal (getf manifest-binding :manifest-sha256) manifest-hash)
                         :screen-manifest-hash))
         (binding-write (vttest-cursor-driver::put
                         (concatenate 'string root "SOURCE-BINDING-PRE.sexp") manifest-binding))
         (exit (invoke-native (getf manifest :backend) root path))
         (receipt (concatenate 'string root "SCREEN-RUN.sexp"))
         (record nil) (hash nil) (read-error nil))
    (declare (ignore binding-check binding-write))
    ;; Validate CURRENT executed source identities both sides of the single invocation.
    (let ((post-error nil))
      (handler-case (validate manifest)
        (error (e) (setf post-error (princ-to-string e))))
      (vttest-cursor-driver::put (concatenate 'string root "SOURCE-BINDING.sexp")
        (append manifest-binding (list :post-run-error post-error)))
      (when post-error (setf read-error post-error)))
    (when (probe-file receipt)
      (handler-case (setf record (vttest-cursor-driver::raw-data receipt)
                          hash (screen-digest receipt))
        (error (e) (setf record nil read-error (princ-to-string e)))))
    (handler-case
        (multiple-value-bind (observations evidence) (summarize-native manifest record exit root)
          (values observations (append (list :native-failed (or read-error (getf evidence :native-failed)))
                              evidence (list :receipt receipt :receipt-sha256 hash
                              :source-binding manifest-binding :receipt-read-error read-error)) nil))
      (error (e)
        (multiple-value-bind (observations evidence) (summarize-native manifest nil exit)
          (values observations (append (list :native-failed t :receipt-error (princ-to-string e))
                        evidence (list :receipt receipt :receipt-sha256 hash
                                       :source-binding manifest-binding)) nil))))))
