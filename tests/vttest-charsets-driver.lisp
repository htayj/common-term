;;;; Native dispatch and honest projection for the complete character-set area.
(defpackage #:vttest-charsets-driver
  (:use #:cl)
  (:export #:make-manifest #:validate #:run #:summarize-native #:invoke-native))
(in-package #:vttest-charsets-driver)

;; The standalone area dispatcher supplies the existing area/cursor utilities.
;; Do not load the recorder here: its entry point owns a disposable application.
(load (merge-pathnames "vttest-charsets-native-contract.lisp" *load-truename*))
(load (merge-pathnames "vttest-charsets-manifest.lisp" *load-truename*))

(defun summarize-native (manifest record process-exit &optional root)
  "Retain counts only after strict original native-contract validation.
An absent or invalid receipt yields an unverified observation, never zero-valued
native counts. The readiness return belongs to RUN, not to a capability receipt."
  (let ((counts nil) (checks nil) (receipt-error nil) (identity nil))
    (handler-case
        (when record
          (vttest-charsets-native-contract:validate-native
           record (getf manifest :backend) root)
          (setf counts (append (loop for key in '(:attempted :reached :captured)
                                     append (list key (getf record key)))
                               (list :stages (copy-tree (getf record :stages))))
                checks (getf record :checks)
                identity t))
      (error (e) (setf receipt-error (princ-to-string e))))
    (unless identity (setf record nil))
    (let ((observations nil) (passed 0) (failed 0) (unverified 0) (verified 0)
          (process-failed (and process-exit (not (member process-exit '(0 3))))))
      (dolist (scenario (getf manifest :scenarios))
        (let* ((capture (first (getf scenario :captures)))
               (check (find (getf capture :pause) checks :key (lambda (c) (getf c :pause))))
               (pixels (or (getf check :pixels) :unverified))
               (bytes (or (getf check :bytes) :unverified))
               (timing (or (getf check :timing) :unverified))
               (states (list pixels timing)))
          ;; Byte comparison is explicitly outside this visible-area obligation.
          (unless (member :unverified states) (incf verified))
          (cond ((member :fail states) (incf failed))
                ((member :unverified states) (incf unverified))
                (t (incf passed)))
          (push (list :scenario (getf scenario :id) :capture (getf capture :id)
                      :fresh (and check (eql process-exit 0) (eq (getf record :status) :pass)
                                  (every (lambda (key) (eql (getf record key) 1))
                                         '(:attempted :reached :captured)))
                      :fixture nil :pixels pixels :bytes bytes :timing timing)
                observations)))
      ;; Valid partial facts cannot turn an incomplete invocation into PASS.
      (when (or receipt-error (not identity) (not (eql process-exit 0))
                (not (eq (getf record :status) :pass)))
        (dolist (observation observations) (setf (getf observation :fresh) nil)))
      (values (nreverse observations)
              (list :native-exit process-exit :native-record record
                    :receipt-error receipt-error
                    :native-failed (or process-failed receipt-error
                                       (and identity
                                            (or (eq (getf record :status) :fail)
                                                (getf record :abort)
                                                (member (getf (getf record :finalization) :state)
                                                        '(:timeout :invalid-result :exited-without-result)))))
                    :counts (append (list :required 1) counts
                                    (unless counts
                                      (list :attempted nil :reached nil :captured nil :stages nil))
                                    (list :verified verified :passed passed
                                          :failed failed :unverified unverified)))))))

(defun invoke-native (backend root manifest-path)
  "Invoke once with the inherited native argv; retain separate streams and exit."
  (let ((script (merge-pathnames "tests/vttest-charsets-native.sh" (truename "."))))
    (unless (probe-file script) (return-from invoke-native nil))
    (let ((arguments (list "--noprofile" "--norc" (namestring script)
                           backend root manifest-path)))
      (vttest-cursor-driver::put (concatenate 'string root "NATIVE-COMMAND.sexp")
        (list :program +charsets-bash+ :arguments arguments
              :input :null :stdout "native.stdout" :stderr "native.stderr"))
      (let* ((process (sb-ext:run-program +charsets-bash+ arguments
                       :search nil :input nil
                       :output (concatenate 'string root "native.stdout")
                       :error (concatenate 'string root "native.stderr")
                       :if-output-exists :error :if-error-exists :error :wait t))
             (exit (sb-ext:process-exit-code process)))
        (vttest-cursor-driver::put (concatenate 'string root "NATIVE-EXIT.sexp")
          (list :exit exit :status (sb-ext:process-status process)))
        exit))))

(defun run (manifest directory manifest-path manifest-hash head tree)
  "Check current source and serialized manifest bytes around one invocation.
Return observations, evidence and NIL readiness, as the generic dispatcher expects."
  (declare (ignore head tree))
  (validate manifest)
  (let* ((root (namestring (truename directory)))
         (input-path (namestring (truename manifest-path)))
         (path (concatenate 'string root "MANIFEST.sexp"))
         (octets (vttest-cursor-driver::bytes input-path))
         (manifest-binding (list :manifest input-path
                                 :manifest-sha256 (vttest-area::sha256 octets)
                                 :native-manifest path
                                 :source-pins (copy-tree (getf manifest :pins))))
         (binding-check
           (progn
             (vttest-area::need (and (> (length root) 1)
                                    (char= (char root (1- (length root))) #\/))
                               :charsets-run-paths)
             (vttest-area::need
              (equal (getf manifest-binding :manifest-sha256) manifest-hash)
              :charsets-manifest-hash)
             ;; Preserve explicit external manifests as exact octets, exclusively.
             ;; The default manifest is already canonical and must not be rewritten.
             (unless (equal input-path path)
               (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                        :if-exists :error :if-does-not-exist :create)
                 (write-sequence octets out)))
             (vttest-area::need (equal (charsets-digest path) manifest-hash)
                               :charsets-native-manifest-hash)))
         (binding-write (vttest-cursor-driver::put
                         (concatenate 'string root "SOURCE-BINDING-PRE.sexp") manifest-binding))
         (exit (invoke-native (getf manifest :backend) root path))
         (receipt (concatenate 'string root "CHARSETS-RUN.sexp"))
         (record nil) (hash nil) (read-error nil))
    (declare (ignore binding-check binding-write))
    (let ((post-error nil))
      (handler-case
          (progn (validate manifest)
                   (let ((input-hash (charsets-digest input-path))
                         (native-hash (charsets-digest path)))
                     (vttest-area::need (and (equal input-hash manifest-hash)
                                            (equal native-hash manifest-hash))
                                       :charsets-manifest-changed)))
        (error (e) (setf post-error (princ-to-string e))))
      (vttest-cursor-driver::put (concatenate 'string root "SOURCE-BINDING.sexp")
        (append manifest-binding (list :post-run-error post-error)))
      (when post-error (setf read-error post-error)))
    (when (probe-file receipt)
      (handler-case
          (setf record (vttest-cursor-driver::raw-data receipt)
                hash (charsets-digest receipt))
        (error (e) (setf record nil read-error (princ-to-string e)))))
    (handler-case
        (multiple-value-bind (observations evidence)
            (summarize-native manifest record exit root)
          (values observations
                  (append (list :native-failed (or read-error (getf evidence :native-failed)))
                          evidence (list :receipt receipt :receipt-sha256 hash
                                         :source-binding manifest-binding
                                         :receipt-read-error read-error))
                  nil))
      (error (e)
        (multiple-value-bind (observations evidence) (summarize-native manifest nil exit)
          (values observations
                  (append (list :native-failed t :receipt-error (princ-to-string e))
                          evidence (list :receipt receipt :receipt-sha256 hash
                                         :source-binding manifest-binding))
                  nil))))))
