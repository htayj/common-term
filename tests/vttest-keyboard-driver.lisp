;;;; Keyboard 5.4/5.5 driver. See the separate native contract.
(defpackage #:vttest-keyboard-driver
  (:use #:cl)
  (:export #:make-manifest #:validate #:run #:summarize-native #:invoke-native))
(in-package #:vttest-keyboard-driver)

(defparameter +bash+
  "/nix/store/kwyi4bq5s2qgwbhz6cs4gjy57c2zdxws-bash-interactive-5.3p20/bin/bash")
(defparameter *adapter* "tests/vttest-keyboard-native.sh")
;; Label, menu, phases, keys, brackets including TAB.
(defparameter +cases+
  '(("keyboard-cursor" (5 4) 3 ("Up" "Down" "Left" "Right") 15)
    ("keyboard-keypad" (5 5) 4 ("KP_1" "KP_2" "KP_Enter") 16)))
(defun case-of (label) (assoc label +cases+ :test #'equal))
(defun capture-ids (label)
  "Non-TAB keys in phase order, then final hold."
  (destructuring-bind (phases keys) (subseq (case-of label) 2 4)
    (append (loop for p below phases
                  append (loop for k in keys
                               collect (format nil "keys~d-~a" p (string-downcase k))))
            '("pause01"))))
(defun digest (path) (vttest-area::sha256 (vttest-cursor-driver::bytes path)))

(load (merge-pathnames "vttest-keyboard-native-contract.lisp" *load-truename*))
(load (merge-pathnames "vttest-keyboard-artifacts.lisp" *load-truename*))
(load (merge-pathnames "vttest-keyboard-timing.lisp" *load-truename*))
(load (merge-pathnames "vttest-keyboard-manifest.lisp" *load-truename*))

(load (merge-pathnames "vttest-keyboard-cleanup.lisp" *load-truename*))
(load (merge-pathnames "vttest-keyboard-native-summary.lisp" *load-truename*))

(defun invoke-native (backend root manifest-path)
  "Invoke the adapter once; a missing adapter is refused."
  (need (member backend '("x11" "wayland") :test #'equal) :unknown-backend)
  (need (and (stringp root) (> (length root) 1) (char= (char root 0) #\/)
             (char= (char root (1- (length root))) #\/)
             (equal root (namestring (truename root)))
             (= (sb-posix:stat-uid (sb-posix:stat root)) (sb-posix:getuid))
             (equal manifest-path (concatenate 'string root "MANIFEST.sexp")))
        :keyboard-native-paths)
  (unless (probe-file *adapter*) (vttest-area::refuse :keyboard-native-adapter-missing 3))
  (let ((arguments (list "--noprofile" "--norc" (namestring (truename *adapter*)) backend
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
  "Return observations, native evidence and NIL readiness."
  (declare (ignore head tree))
  (validate manifest)
  (let* ((root (namestring (truename directory)))
         (input-path (namestring (truename manifest-path)))
         (path (concatenate 'string root "MANIFEST.sexp"))
         (receipt (concatenate 'string root +receipt+))
         (record nil) (read-error nil))
    (need (equal (digest input-path) manifest-hash) :keyboard-manifest-hash)
    (unless (equal input-path path)
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                :if-exists :error :if-does-not-exist :create)
        (write-sequence (vttest-cursor-driver::bytes input-path) out)))
    (need (equal (digest path) manifest-hash) :keyboard-native-manifest-hash)
    (let ((exit (invoke-native (getf manifest :backend) root path)))
      (handler-case
          (progn (validate manifest)
                 (need (equal (digest path) manifest-hash) :keyboard-manifest-changed)
                 (when (probe-file receipt)
                   (setf record (vttest-area:read-data receipt))
                   (need (equal (getf record :manifest-sha256) manifest-hash)
                         :keyboard-receipt-manifest-mismatch)))
        (error (e) (setf record nil read-error (princ-to-string e))))
      (multiple-value-bind (observations evidence) (summarize-native manifest record exit root)
        (when read-error (dolist (o observations) (setf (getf o :fresh) nil)))
        (values observations
                (append (list :native-failed (or (and read-error t) (getf evidence :native-failed))
                              :receipt-read-error read-error)
                        evidence)
                nil)))))

