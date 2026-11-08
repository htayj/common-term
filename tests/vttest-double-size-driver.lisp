;;;; Standalone area-driver plumbing. No product or recorder is loaded here.
(defpackage #:vttest-double-size-driver
  (:use #:cl)
  (:export #:make-manifest #:validate #:run #:summarize-native #:invoke-native))
(in-package #:vttest-double-size-driver)
(load (merge-pathnames "vttest-double-size-native-contract.lisp" *load-truename*))

(defparameter +bash+
  "/nix/store/kwyi4bq5s2qgwbhz6cs4gjy57c2zdxws-bash-interactive-5.3p20/bin/bash")
(defun digest (path)
  ;; Canonical manifest hashing: hash original file octets, not printed forms.
  (vttest-area::sha256 (vttest-cursor-driver::bytes path)))

(defun summarize-native (manifest record process-exit &optional root)
  "Project only counts admitted by the original strict area contract.
Observation identities are declarations, not evidence of captures. Unknown
native counts remain NIL; separate recorder/native stages retain their basis."
  (let ((counts nil) (checks nil) (receipt-error nil) (identity nil))
    (handler-case
        (when record
          (vttest-double-size-native-contract:validate-native
            record (getf manifest :backend) root)
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
                      :fresh (and check (eql process-exit 0)
                                  (eq (getf record :status) :pass)
                                  (every (lambda (key) (eql (getf record key) 6))
                                         '(:attempted :reached :captured)))
                      :fixture nil :pixels pixels :bytes bytes :timing timing)
                observations)))
      (values (nreverse observations)
              (list :native-exit process-exit :native-record record
                    :receipt-error receipt-error
                    :native-failed (or process-failed receipt-error
                       (and identity
                            (or (eq (getf record :status) :fail) (getf record :abort)
                                (member (getf (getf record :finalization) :state)
                                        '(:timeout :invalid-result :exited-without-result)))))
                    :counts (append (list :required 6) counts
                              (unless counts
                                (list :attempted nil :reached nil :captured nil :stages nil))
                              (list :verified verified :passed passed :failed failed
                                    :unverified unverified)))))))

(defun invoke-native (backend root manifest-path)
  "Invoke exactly once. ROOT is canonical absolute with a trailing slash.
The shell receives BACKEND, absolute run without slash, absolute MANIFEST.sexp."
  (vttest-area::need (member backend '("x11" "wayland") :test #'equal) :unknown-backend)
  (vttest-area::need (and (stringp root) (> (length root) 1)
                         (char= (char root 0) #\/)
                         (char= (char root (1- (length root))) #\/)
                         (equal root (namestring (truename root)))
                         (equal manifest-path (concatenate 'string root "MANIFEST.sexp")))
                    :double-size-native-paths)
  (let* ((script (namestring (truename "tests/vttest-double-size-native.sh")))
         (arguments (list "--noprofile" "--norc" script backend
                          (subseq root 0 (1- (length root))) manifest-path)))
    (vttest-cursor-driver::put (concatenate 'string root "NATIVE-COMMAND.sexp")
      (list :program +bash+ :arguments arguments :input :null
            :stdout "native.stdout" :stderr "native.stderr"))
    (let* ((process (sb-ext:run-program +bash+ arguments :search nil :input nil
                      :output (concatenate 'string root "native.stdout")
                      :error (concatenate 'string root "native.stderr")
                      :if-output-exists :error :if-error-exists :error :wait t))
           (exit (sb-ext:process-exit-code process)))
      (vttest-cursor-driver::put (concatenate 'string root "NATIVE-EXIT.sexp")
        (list :exit exit :status (sb-ext:process-status process)))
      exit)))

(defun run (manifest directory manifest-path manifest-hash head tree)
  "Return observations, original native evidence, and NIL readiness.
A complete resolved execution-input freeze is an additional native release gate."
  (declare (ignore head tree))
  (validate manifest)
  (let* ((root (namestring (truename directory)))
         (input-path (namestring (truename manifest-path)))
         (path (concatenate 'string root "MANIFEST.sexp"))
         (binding (list :manifest input-path :manifest-sha256 (digest input-path)
                        :native-manifest path :source-pins (copy-tree (getf manifest :pins))))
         (receipt (concatenate 'string root "DOUBLE-SIZE-RUN.sexp"))
         (record nil) (hash nil) (read-error nil))
    (vttest-area::need (and (> (length root) 1)
                           (char= (char root (1- (length root))) #\/))
                      :double-size-run-paths)
    (vttest-area::need (equal (getf binding :manifest-sha256) manifest-hash)
                      :double-size-manifest-hash)
    ;; Explicit public manifests may be outside the owned run. Preserve their
    ;; original bytes in the shell's required canonical location, exclusively.
    (unless (equal input-path path)
      (let ((octets (vttest-cursor-driver::bytes input-path)))
        (vttest-area::need (equal (vttest-area::sha256 octets) manifest-hash)
                          :double-size-input-manifest-changed)
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :error :if-does-not-exist :create)
          (write-sequence octets out))))
    (vttest-area::need (equal (digest path) manifest-hash) :double-size-native-manifest-hash)
    (vttest-cursor-driver::put (concatenate 'string root "SOURCE-BINDING-PRE.sexp") binding)
    (let ((exit (invoke-native (getf manifest :backend) root path)))
      ;; Rehash current declarations and the same original manifest after launch.
      (handler-case
          (progn (validate manifest)
                 (vttest-area::need (and (equal (digest input-path) manifest-hash)
                                        (equal (digest path) manifest-hash))
                                   :double-size-manifest-changed))
        (error (e) (setf read-error (princ-to-string e))))
      (vttest-cursor-driver::put (concatenate 'string root "SOURCE-BINDING.sexp")
        (append binding (list :post-run-error read-error)))
      (when (probe-file receipt)
        (handler-case
            (let ((original (vttest-double-size-native-contract:read-data receipt)))
              (vttest-area::need (equal (getf original :manifest-sha256) manifest-hash)
                                :double-size-receipt-manifest-mismatch)
              (setf record original hash (digest receipt)))
          (error (e) (setf record nil read-error (princ-to-string e)))))
      (multiple-value-bind (observations evidence)
          (summarize-native manifest record exit root)
        (when read-error
          (dolist (o observations) (setf (getf o :fresh) nil)))
        (values observations
                (append (list :native-failed (or read-error (getf evidence :native-failed)))
                        evidence (list :receipt receipt :receipt-sha256 hash
                                       :source-binding binding :receipt-read-error read-error))
                nil)))))

(load (merge-pathnames "vttest-double-size-manifest.lisp" *load-truename*))
