;;;; One directly owned gate child; observations precede post-close claims.
(in-package #:vttest-keyboard-driver)

;; Bound only by explicitly invoked offline controls, never by environment input.
(defparameter *keyboard-test-gate* nil)
(defun native-path (root name) (concatenate 'string root name))
(defun native-put (path value)
  "Write ordinary transport data without preserving string array specialization."
  (with-open-file (s path :direction :output :if-exists :error
                          :if-does-not-exist :create :external-format :utf-8)
    (let ((*print-readably* nil) (*print-escape* t) (*print-pretty* nil)
          (*print-circle* nil) (*print-length* nil) (*print-level* nil)
          (*print-base* 10) (*print-radix* nil) (*print-case* :upcase)
          (*print-array* t) (*print-gensym* t)
          (*package* (find-package :cl)) (*readtable* (copy-readtable nil)))
      (write value :stream s)
      (terpri s))))
(defun native-text (path) (vttest-cursor-driver::text path))
(defun native-present-p (path)
  (handler-case (progn (sb-posix:lstat path) t)
    (sb-posix:syscall-error (e)
      (if (= (sb-posix:syscall-errno e) sb-posix:enoent) nil (error e)))))
(defun native-start (pid)
  (handler-case
      (let* ((s (native-text (format nil "/proc/~d/stat" pid)))
             (end (position #\) s :from-end t))
             (fields (and end (uiop:split-string (subseq s (+ end 2))
                                               :separator '(#\Space #\Newline)))))
        (let ((token (nth 19 fields)))
          (and (not (member (first fields) '("Z" "X") :test #'equal))
               token (plusp (length token)) (every #'digit-char-p token)
               (plusp (parse-integer token)) token)))
    (error () nil)))
(defun native-runtime ()
  "Use the existing reviewed binary/font pins and packaged SBCL path."
  (unless (every #'probe-file '(".build/deps/share/fonts/terminus/ter-u16n.otb" ".build/test-tools/bin/vttest" ".build/deps/bin/sbcl"))
    (vttest-area::refuse :keyboard-runtime-missing 3))
  (let ((font (namestring (truename ".build/deps/share/fonts/terminus/ter-u16n.otb")))
        (binary (namestring (truename ".build/test-tools/bin/vttest")))
        (runtime (namestring (truename ".build/deps/bin/sbcl"))))
    (need (and (equal (digest binary) vttest-cursor-driver::+binary+)
               (equal (digest font) vttest-cursor-driver::+font+)
               (plusp (logand #o111 (sb-posix:stat-mode (sb-posix:stat runtime)))))
          :keyboard-runtime-pins)
    (values font binary runtime)))
(defun keyboard-gate-environment (backend root request font binary runtime)
  (append
   '("PATH=/run/current-system/sw/bin:/usr/bin:/bin")
   (loop for (key name) on '("HOME" "home" "XDG_CACHE_HOME" "cache"
                            "XDG_CONFIG_HOME" "config" "XDG_DATA_HOME" "data"
                            "TMPDIR" "tmp") by #'cddr
         collect (format nil "~a=~a~a" key root name))
   (list (format nil "LISPTERM_FONT=~a" font) (format nil "VTTEST=~a" binary)
         (format nil "LISPTERM_SBCL=~a" runtime)
         "LISPTERM_VTTEST_EXACT_LABELS=keyboard-cursor,keyboard-keypad"
         "LISPTERM_VTTEST_MODE=record" "LISPTERM_VTTEST_SCALE=1"
         "LISPTERM_VTTEST_TIMEOUT=120"
         (format nil "LISPTERM_VTTEST_AREA_REQUEST=~a" request)
         (format nil "LISPTERM_VTTEST_AREA_RECEIPT=~alaunch-receipt.txt" root)
         (format nil "LISPTERM_VTTEST_ARTIFACT_ROOT=~arecorder" root))
   (when (equal backend "wayland")
     (list (format nil "LISPTERM_WAYLAND_ARTIFACTS=~awayland-artifacts" root)))))
(defun keyboard-preclose-title (root pid start)
  "Only accept one exact fixture title while this same child is alive."
  (let ((receipt (native-path root "recorder/cleanup-receipt.sexp"))
        (fixture (native-path root "recorder/fixture.txt")))
    (when (and start (equal start (native-start pid))
               (not (native-present-p receipt)) (native-present-p fixture))
      (let* ((lines (uiop:split-string (native-text fixture) :separator '(#\Newline)))
             (titles (remove-if-not (lambda (s) (uiop:string-prefix-p "title=" s)) lines)))
        (when titles
          (need (= 1 (length titles)) :keyboard-duplicate-title)
          (let* ((title (subseq (first titles) 6))
                 (prefix "Lispterm-vttest-") (suffix (format nil "-~d" pid))
                 (n (length prefix)))
            (need (and (= (length title) (+ n 16 (length suffix)))
                       (uiop:string-prefix-p prefix title)
                       (equal (subseq title (+ n 16)) suffix)
                       (every (lambda (c) (find c "0123456789abcdef"))
                              (subseq title n (+ n 16)))) :keyboard-title)
            (when (and (equal start (native-start pid)) (not (native-present-p receipt)))
              title)))))))
(load (merge-pathnames "vttest-keyboard-child.lisp" *load-truename*))

(defun launch-keyboard-gate (backend root request-path)
  "Return frozen launch PLIST, independently joined exit PLIST, diagnostics."
  (multiple-value-bind (font binary runtime) (native-runtime)
    (let* ((hash (digest request-path))
           (env (keyboard-gate-environment backend root request-path font binary runtime))
           (args (list "--noprofile" "--norc"
                       (or *keyboard-test-gate* (namestring (truename "tests/vttest-gate.sh")))
                       backend))
           (errors nil) (expected nil) (actual-gate-exit nil))
      (dolist (name '("home" "cache" "config" "data" "tmp"))
        (sb-posix:mkdir (native-path root name) #o700))
      (when (equal backend "wayland")
        (sb-posix:mkdir (native-path root "wayland-artifacts") #o700))
      (native-put (native-path root "GATE-COMMAND.sexp")
                  (list :program +bash+ :arguments args :environment env :input :null
                        :stdout "gate.stdout" :stderr "gate.stderr"))
      (let* ((child (sb-ext:run-program +bash+ args :search nil :environment env
                     :input nil :output (native-path root "gate.stdout")
                     :error (native-path root "gate.stderr")
                     :if-output-exists :error :if-error-exists :error :wait nil))
             (pid (sb-ext:process-pid child)) (start (native-start pid)))
        (multiple-value-bind (title code waited issues)
            (keyboard-child-lifecycle child root pid start)
          (setf errors issues)
          (when (and start title)
            (setf expected (list :kind :vttest-keyboard-gate-launch :version 1 :backend backend
                                 :gate-pid pid :gate-start start :title title
                                 :artifacts (native-path root "recorder")
                                 :area-request-sha256 hash)))
          (when (and start waited (typep code '(integer 0 255)))
            (setf actual-gate-exit
                  (list :kind :vttest-keyboard-gate-exit :version 1 :backend backend
                        :gate-pid pid :gate-start start :exit-code code :wait-observed t)))
          ;; Record writes must not discard the independent in-memory observations.
          (handler-case
              (progn
                (when expected (native-put (native-path root "GATE-LAUNCH.sexp") expected))
                (when actual-gate-exit
                  (native-put (native-path root "GATE-EXIT.sexp") actual-gate-exit)))
            (error (e) (push (princ-to-string e) errors)))
          (unless start (push :missing-gate-start errors))
          (unless title (push :missed-preclose-title errors))
          (unless actual-gate-exit (push :missing-gate-exit errors))))
      (values expected actual-gate-exit errors))))
