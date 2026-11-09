;;;; One native VT52 invocation followed by fresh original-evidence validation.
(defpackage #:vttest-vt52-driver
  (:use #:cl)
  (:export #:make-manifest #:validate #:run #:invoke-native #:summarize-native #:put))
(in-package #:vttest-vt52-driver)
(load (merge-pathnames "vttest-vt52-native-contract.lisp" *load-truename*))
(load (merge-pathnames "vttest-vt52-manifest.lisp" *load-truename*))
(defparameter *source-root*
  (truename (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*))))

(defun put (path value)
  "Write one bounded ordinary-string receipt, exclusively."
  (let* ((*print-readably* nil) (*print-escape* t) (*print-base* 10) (*print-radix* nil)
         (*print-level* nil) (*print-length* nil) (*print-circle* nil) (*print-pretty* nil)
         (text (with-output-to-string (s) (write value :stream s) (terpri s)))
         (bytes (sb-ext:string-to-octets text :external-format :utf-8)))
    (vttest-area::need (<= (length bytes)
                           (if (eq (getf value :kind) :vttest-area-manifest)
                               vttest-area::+limit+ 8192)) :vt52-receipt-too-large)
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                           :if-exists :error :if-does-not-exist :create)
      (write-sequence bytes s)) path))

(defun invoke-native (backend root manifest-path)
  (let* ((script (merge-pathnames "tests/vttest-charsets-native.sh" *source-root*))
         (arguments (list "--noprofile" "--norc" (namestring script)
                          backend root manifest-path "--profile" "vt52")))
    (put (concatenate 'string root "NATIVE-COMMAND.sexp")
         (list :program +vt52-bash+ :arguments arguments :input :null
               :stdout "native.stdout" :stderr "native.stderr"))
    (let* ((process (sb-ext:run-program +vt52-bash+ arguments :search nil :input nil
                     :output (concatenate 'string root "native.stdout")
                     :error (concatenate 'string root "native.stderr")
                     :if-output-exists :error :if-error-exists :error :wait t))
           (exit (sb-ext:process-exit-code process)))
      (put (concatenate 'string root "NATIVE-EXIT.sexp")
           (list :exit exit :status (sb-ext:process-status process))) exit)))

(defun summarize-native (manifest process-exit root &optional binding-error)
  "Recompute the outer contract. Stored native PASS is insufficient."
  (let ((report nil) (validation-error nil))
    (handler-case
        (setf report (vttest-vt52-native-contract:validate-vt52-native
                      root :backend (getf manifest :backend) :scale (getf manifest :scale)))
      (error (e) (setf validation-error (princ-to-string e))))
    (let* ((counts (getf report :counts))
           (fresh (and (null binding-error) (null validation-error) (eql process-exit 0)
                       (eq (getf report :verdict) :pass) (eql (getf report :valid-frames) 1)
                       (every (lambda (k) (eql (getf counts k) 1))
                              '(:required :attempted :reached :captured :verified :passed))
                       (every (lambda (k) (eql (getf counts k) 0)) '(:failed :unverified))))
           (scenario (first (getf manifest :scenarios)))
           (capture (first (getf scenario :captures))))
      (values
       (list (list :scenario (getf scenario :id) :capture (getf capture :id)
                   :fresh (not (null fresh)) :fixture nil :bytes :unverified
                   :pixels (if fresh :pass :unverified) :timing (if fresh :pass :unverified)))
       (list :native-exit process-exit :native-report report :receipt-error validation-error
             :source-binding-error binding-error
             :native-failed (or binding-error validation-error
                                (and process-exit (not (eql process-exit 0)))
                                (eq (getf report :verdict) :refuse))
             :counts (if fresh counts
                         (list :required 1 :attempted (getf counts :attempted)
                               :reached (getf counts :reached) :captured (getf counts :captured)
                               :verified (getf counts :verified) :passed 0
                               :failed (getf counts :failed) :unverified 1)))
       nil))))

(defun run (manifest directory manifest-path manifest-hash head tree)
  (declare (ignore head tree))
  (validate manifest)
  (let* ((root (namestring (truename directory)))
         (input (namestring (truename manifest-path)))
         (path (concatenate 'string root "MANIFEST.sexp"))
         (octets (vttest-cursor-driver::bytes input))
         (binding (list :manifest input :manifest-sha256 (vttest-area::sha256 octets)
                        :native-manifest path
                        :source-pins (list :count (length (getf manifest :pins))
                                           :sha256 (vttest-area::sha256
                                            (sb-ext:string-to-octets
                                             (let ((*print-readably* nil) (*print-escape* t)
                                                   (*print-base* 10) (*print-radix* nil)
                                                   (*print-level* nil) (*print-length* nil)
                                                   (*print-pretty* nil) (*print-circle* nil))
                                               (write-to-string (getf manifest :pins)))
                                             :external-format :utf-8)))))
         (post-error nil))
    (vttest-area::need (and (> (length root) 1) (char= (char root (1- (length root))) #\/))
                      :vt52-run-paths)
    (vttest-area::need (equal (getf binding :manifest-sha256) manifest-hash) :vt52-manifest-hash)
    (unless (equal input path)
      (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                             :if-exists :error :if-does-not-exist :create)
        (write-sequence octets s)))
    (vttest-area::need (equal (vt52-digest path) manifest-hash) :vt52-native-manifest-hash)
    (put (concatenate 'string root "SOURCE-BINDING-PRE.sexp") binding)
    (let ((exit (invoke-native (getf manifest :backend) root path)))
      (handler-case
          (progn (validate manifest)
                 (vttest-area::need
                  (and (equal (vt52-digest input) manifest-hash)
                       (equal (vt52-digest path) manifest-hash)) :vt52-manifest-changed))
        (error (e) (setf post-error (princ-to-string e))))
      (put (concatenate 'string root "SOURCE-BINDING.sexp")
           (append binding (list :post-run-error post-error)))
      (multiple-value-bind (observations evidence readiness)
          (summarize-native manifest exit root post-error)
        (values observations
                (append evidence
                        (list :receipt (concatenate 'string root "VT52-RUN.sexp")
                              :source-binding binding)) readiness)))))
