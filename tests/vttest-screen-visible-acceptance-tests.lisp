;;;; Product-free controls for the actual visible-screen acceptance boundaries.
(require :sb-posix)
(in-package :cl-user)
(defparameter *workspace* #p"/home/taylor/projects/lispterm/")
(defparameter *attempt* (pathname (sb-ext:posix-getenv "SCREEN_VISIBLE_ATTEMPT")))
(defparameter *checks* nil)
(defun check (name value)
  (unless value (error "Visible-screen control failed: ~s" name))
  (push name *checks*) t)
(defun bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (assert (= (length b) (read-sequence b s))) b)))
(defun text (path) (sb-ext:octets-to-string (bytes path) :external-format :utf-8))
(defun put (path value)
  (with-open-file (s path :direction :output :if-exists :error)
    (let ((*print-readably* nil) (*print-pretty* nil)) (write value :stream s) (terpri s))))
(defun put-text (path value)
  (with-open-file (s path :direction :output :if-exists :error) (write-string value s)))
(defparameter *sources*
  '("vttest-selection.lisp" "vttest-area.lisp" "vttest-cursor-driver.lisp"
    "vttest-cursor-oracle.lisp" "vttest-screen-oracle.lisp" "vttest-screen-frame.lisp"
    "vttest-screen-native-contract.lisp" "vttest-screen-pixels.lisp"
    "vttest-screen-recorder.lisp" "vttest-screen-manifest.lisp"))
;; These are reader scaffolds, not product definitions. Shadow inherited OPEN/CLOSE.
(dolist (name '("LISPTERM" "LISPTERM.ENGINE" "LISPTERM.PTY" "LISPTERM.DISPLAY.WAYLAND"
                "VTTEST-SELECTION" "VTTEST-AREA" "VTTEST-CURSOR-DRIVER" "VTTEST-CURSOR-ORACLE"
                "VTTEST-SCREEN-ORACLE" "VTTEST-SCREEN-FRAME" "VTTEST-SCREEN-NATIVE-CONTRACT"
                "VTTEST-SCREEN-PIXELS" "VTTEST-SCREEN-RECORDER" "VTTEST-SCREEN-DRIVER" "UIOP" "ASDF"
                "VTTEST-PROTOCOL" "VTTEST-CHECK"))
  (unless (find-package name) (make-package name :use '("CL"))))
(defun constituent-p (c) (or (alphanumericp c) (find c "-._*+")))
(defun scaffold (string)
  (loop for i from 1 below (1- (length string)) when (char= #\: (char string i)) do
    (let* ((start (loop for j downfrom (1- i) to 0 while (constituent-p (char string j)) finally (return (1+ j))))
           (package (and (< start i) (find-package (string-upcase (subseq string start i)))))
           (end (loop for j from (1+ i) below (length string) while (constituent-p (char string j)) finally (return j))))
      (when (and package (> end (1+ i))
                 (not (member (package-name package) '("COMMON-LISP" "SB-POSIX" "SB-EXT" "SB-UNIX" "SB-SYS") :test #'equal)))
        (let ((name (string-upcase (subseq string (1+ i) end))))
          (when (eq :inherited (nth-value 1 (find-symbol name package))) (shadow name package))
          (export (intern name package) package))))))
(dolist (file *sources*) (scaffold (text (merge-pathnames (concatenate 'string "current/" file) *attempt*))))
(defun read-source (file)
  (let ((*read-eval* nil) (*package* (find-package :cl-user)) (pairs nil))
    (with-open-file (s (merge-pathnames (concatenate 'string "current/" file) *attempt*))
      (loop for form = (read s nil :eof) until (eq form :eof) do
        (check (list :top-level file (car form))
               (and (consp form) (symbolp (car form))
                    (member (symbol-name (car form))
                            '("REQUIRE" "LOAD" "LOAD-SYSTEM" "DEFPACKAGE" "IN-PACKAGE" "EXPORT"
                              "DEFUN" "DEFPARAMETER" "DEFCONSTANT" "DEFVAR" "DEFINE-CONDITION"
                              "DEFSTRUCT" "DEFMACRO" "WHEN" "MAIN" "DEFINE-ALIEN-ROUTINE") :test #'equal)))
        (cond ((eq 'defpackage (car form)) (eval form))
              ((eq 'in-package (car form))
               (setf *package* (find-package (second form)))
               (check (list :source-package file)
                      (equal (package-name *package*)
                             (if (equal file "vttest-screen-manifest.lisp") "VTTEST-SCREEN-DRIVER"
                                 (string-upcase (pathname-name file))))))
              ((eq 'export (car form)) (eval form)))
        (when (member (car form) '(defun defparameter defconstant defvar define-condition defstruct defmacro))
          (let ((name (if (consp (second form)) (first (second form)) (second form))))
            (check (list :scope file name) (eq (symbol-package name) *package*))))
        (when (eq (car form) 'sb-alien:define-alien-routine)
          (check :exact-area-openat-definition
                 (and (equal file "vttest-area.lisp")
                      (equal '("openat" vttest-area::%openat) (second form)))))
        (push (list *package* form) pairs)))
    (nreverse pairs)))
(defparameter *forms* (loop for file in *sources* collect (cons file (read-source file))))
(defun definitions (file) (cdr (assoc file *forms* :test #'equal)))
(defun named (file name)
  (or (find name (definitions file) :test #'equal
            :key (lambda (p) (let ((form (second p)))
                              (and (consp form) (symbolp (second form)) (symbol-name (second form))))))
      (error "Missing source definition ~a/~a" file name)))
(defun admit (pair) (let ((*package* (first pair))) (eval (second pair))))
;; All source has been read and checked before any function is admitted.
(dolist (file *sources*)
  (check (list :source-equality file)
         (equalp (bytes (merge-pathnames (concatenate 'string "tests/" file) *workspace*))
                 (bytes (merge-pathnames (concatenate 'string "current/" file) *attempt*)))))
(dolist (file '("vttest-selection.lisp" "vttest-area.lisp" "vttest-screen-frame.lisp"
                "vttest-screen-native-contract.lisp" "vttest-screen-pixels.lisp" "vttest-screen-manifest.lisp"
                "vttest-screen-oracle.lisp" "vttest-cursor-oracle.lisp"))
  (dolist (pair (definitions file))
    (when (member (car (second pair)) '(defun defmacro defparameter defconstant defvar defstruct define-condition))
      (unless (or (and (equal file "vttest-cursor-oracle.lisp")
                       (member (second (second pair)) '(vttest-cursor-oracle:compare-png)))
                  (and (equal file "vttest-screen-manifest.lisp")
                       (equal "INVOKE-NATIVE" (symbol-name (second (second pair))))))
        (admit pair)))))
(admit (find-if (lambda (p) (eq 'sb-alien:define-alien-routine (car (second p))))
                (definitions "vttest-area.lisp")))
(dolist (name '("NEED" "BYTES" "TEXT" "PUT" "RAW-PREFLIGHT" "RAW-DATA"))
  (admit (named "vttest-cursor-driver.lisp" name)))
(defvar vttest-screen-recorder::*art*)
(dolist (name '("PATH" "SAVE" "TOUCH" "PAUSE-NAME"))
  (admit (named "vttest-screen-recorder.lisp" name)))
(defparameter *historical* (merge-pathnames ".build/evidence/vttest-area-runs/run-GI3nuS/recorder/" *workspace*))
(defparameter *historical-pixels* (merge-pathnames ".build/evidence/screen-area-retained-pixel-comparison-20261107-a/" *workspace*))
(defun refuse-p (thunk) (handler-case (progn (funcall thunk) nil) (error () t)))
(defun fixture-file (root relative value)
  (vttest-screen-native-contract:write-data (concatenate 'string root relative) value))
(defun link-original (old new)
  (check :new-link-absent (not (probe-file new)))
  (sb-posix:link (namestring old) new))
(defun walk-matches (tree predicate)
  (let ((found nil))
    (labels ((walk (x) (when (consp x) (when (funcall predicate x) (push x found))
                                    (walk (car x)) (walk (cdr x))))) (walk tree))
    (nreverse found)))
(defparameter *result-slice*
  (let ((slices (walk-matches (second (named "vttest-screen-recorder.lisp" "RUN-MAIN"))
                  (lambda (x) (and (eq 'let (car x)) (consp (second x))
                                   (consp (first (second x)))
                                   (equal "RESULT" (symbol-name (first (first (second x))))))))))
    (check :one-actual-result-writer (= 1 (length slices))) (first slices)))
(defparameter *fixture-record* nil)
(defparameter *fixture-root* nil)
(defun make-fixture (root)
  "Use historical image bytes as labeled fixture data, not fresh backend evidence."
  (ensure-directories-exist (concatenate 'string root "recorder/.reserve"))
  (ensure-directories-exist (concatenate 'string root "native/.reserve"))
  (let ((checks nil))
    (loop for n from 1 to 15 do
      (let* ((stem (format nil "recorder/pause-~2,'0d-" n))
             (short (format nil "pause-~2,'0d-" n))
             (width (if (member n '(3 5)) 1056 640)))
        (dolist (suffix '("before.sexp" "ready.sexp" "pre.sexp" "final.sexp" "native.sexp" "request.sexp"))
          (fixture-file root (concatenate 'string stem suffix)
                        (vttest-screen-native-contract:read-data (merge-pathnames (concatenate 'string short suffix) *historical*))))
        (link-original (merge-pathnames (concatenate 'string short "actual.png") *historical*)
                       (concatenate 'string root stem "actual.png"))
        (link-original (merge-pathnames (concatenate 'string short "actual.rgba") *historical-pixels*)
                       (concatenate 'string root stem "actual.rgba"))
        (let* ((native (vttest-screen-native-contract:read-data (concatenate 'string root stem "native.sexp")))
               (old (vttest-screen-native-contract:read-data (merge-pathnames (concatenate 'string short "comparison.sexp") *historical-pixels*)))
               (commands nil))
          (loop for action in '("identify" "decode") do
            (let ((base (concatenate 'string stem action)))
              (fixture-file root (concatenate 'string base ".argv.sexp")
                (if (equal action "identify")
                    (list vttest-screen-pixels::+decoder+ "identify" "-format" "%w %h" (concatenate 'string root stem "actual.png"))
                    (list vttest-screen-pixels::+decoder+ (concatenate 'string root stem "actual.png") "-depth" "8"
                          (concatenate 'string "rgba:" root stem "actual.rgba"))))
              (put-text (concatenate 'string root base ".stdout") (if (equal action "identify") (format nil "~d 384" width) ""))
              (put-text (concatenate 'string root base ".stderr") "")
              (fixture-file root (concatenate 'string base ".exit") 0)
              (push (loop for suffix in '(".argv.sexp" ".stdout" ".stderr" ".exit") collect
                          (vttest-screen-native-contract:reference root (concatenate 'string base suffix))) commands)))
          (fixture-file root (concatenate 'string stem "comparison.sexp")
            (list :version 1 :pause n :png (vttest-screen-native-contract:reference root (concatenate 'string stem "actual.png"))
                  :phase t :width width :height 384
                  :font (list :path vttest-screen-pixels::+font+ :sha256 vttest-screen-pixels::+font-sha+)
                  :decoder (list :path vttest-screen-pixels::+decoder+ :sha256 vttest-screen-pixels::+decoder-sha+)
                  :bracket (loop for suffix in '("before.sexp" "ready.sexp" "pre.sexp" "final.sexp" "native.sexp") collect
                                 (vttest-screen-native-contract:reference root (concatenate 'string stem suffix)))
                  :identify (second commands) :decode (first commands)
                  :rgba (vttest-screen-native-contract:reference root (concatenate 'string stem "actual.rgba"))
                  :decoded-length (* width 384 4) :opaque t :comparison (getf (getf old :result) :comparison)))
          (push (list :pause n :pixels :pass :bytes :unverified :timing :pass :frame :pass
                      :target :recognized-navigation-only :native native
                      :comparison (vttest-screen-native-contract:reference root (concatenate 'string stem "comparison.sexp"))) checks))
        (put-text (concatenate 'string root stem "request") "Fixture request binding.\n")
        (fixture-file root (concatenate 'string stem "capture.exit") 0)))
    (setf checks (nreverse checks))
    (let* ((*package* (find-package :vttest-screen-recorder))
           (vttest-screen-recorder::*art* (pathname (concatenate 'string root "recorder/"))))
      (declare (special vttest-screen-recorder::*art*))
      (check :actual-recorder-writer-zero
        (eql 0 (eval `(let ((vttest-screen-recorder::failure nil)
                           (vttest-screen-recorder::attempted 15) (vttest-screen-recorder::reached 15)
                           (vttest-screen-recorder::captured 15) (vttest-screen-recorder::accounting :pass)
                           (vttest-screen-recorder::cleanup :graceful-exit)
                           (vttest-screen-recorder::*backend* :x11)
                           (vttest-screen-recorder::checks ',checks))
                       (declare (special vttest-screen-recorder::*backend*)) ,*result-slice*)))))
    (setf *fixture-record* (vttest-screen-native-contract:read-data (concatenate 'string root "recorder/RESULT.sexp")))
    (check :actual-original-import-pass (eq :pass (vttest-screen-native-contract:import-recorder root "x11")))
    (with-open-file (s (concatenate 'string root "native/stage-events.sexp") :direction :output :if-exists :error)
      (loop for n from 1 to 15 do (format s "(~d :REQUEST)~%(~d :INVOKED)~%(~d :COMPLETED 0)~%" n n n)))
    (put-text (concatenate 'string root "native/unowned-endpoints.txt") "")
    (fixture-file root "native/client.exit" 0)
    (fixture-file root "native/finalization-clock.sexp"
      (list :clock :clock-monotonic :limit-seconds 15 :start-ns 100 :end-ns 200 :state :exited :pid 1 :token "fixture" :run root :backend "x11"))
    (fixture-file root "native/import-deadline.sexp"
      (list :clock :clock-monotonic :deadline-ns 15000000100 :validated-ns 300 :state :valid))
    (loop for (key value) on (list "ROOT" root "BACKEND" "x11" "FINALIZATION" ":COMPLETE" "CLIENT_EXIT" "0"
                                  "STATUS" ":PASS" "ATTEMPTED" "15" "REACHED" "15" "CAPTURED" "15"
                                  "BUILD" ":PASS" "STARTED" "T" "CLEANUP" ":PASS" "REASON" "Historical-input visible-screen fixture.") by #'cddr
          do (sb-posix:setenv (concatenate 'string "LISPTERM_SCREEN_" key) value 1))
    (check :actual-native-publication-pass (eq :pass (vttest-screen-native-contract:publish-native)))
    (setf *fixture-root* root) 0))
;; Only native invocation is replaced for the full generic command path.
(setf (symbol-function 'vttest-screen-driver:invoke-native)
      (lambda (backend root path) (declare (ignore path)) (assert (equal backend "x11")) (make-fixture root)))
(check :actual-area-main-zero (eql 0 (vttest-area:main '("--area" "screen" "--backend" "x11"))))
(defparameter *native* (vttest-screen-native-contract:read-data (concatenate 'string *fixture-root* "SCREEN-RUN.sexp")))
(defparameter *outer* (vttest-screen-native-contract:read-data (concatenate 'string *fixture-root* "RESULT.sexp")))
(check :actual-area-recorded-pass (and (eq :pass (getf *outer* :outcome)) (eql 0 (getf *outer* :exit))
                                      (eql 15 (getf *outer* :passed-count)) (eql 0 (getf *outer* :unverified-count))))
(check :bytes-honestly-unverified (every (lambda (c) (eq :unverified (getf c :bytes))) (getf *fixture-record* :checks)))
;; Fail closed at the actual recorder schema boundary before any expensive file traversal.
(dolist (case '(:pixel :subset :duplicate :frame :close :forged :missing-comparison))
  (let ((r (copy-tree *fixture-record*)))
    (case case
      (:pixel (setf (getf (first (getf r :checks)) :pixels) :fail))
      (:subset (setf (getf r :checks) (butlast (getf r :checks))))
      (:duplicate (setf (getf (second (getf r :checks)) :pause) 1))
      (:frame (setf (getf (first (getf r :checks)) :frame) :fail))
      (:close (setf (getf r :cleanup) '(:graceful-exit :close-error "Injected close error.")))
      (:forged (setf (getf r :captured) 14))
      (:missing-comparison (remf (first (getf r :checks)) :comparison)))
    (check (list :recorder-reject case) (refuse-p (lambda () (vttest-screen-native-contract:validate-recorder r "x11"))))))
;; These mutations cross the actual native validator and manifest/area aggregation.
(dolist (case '(:recorder-exit :abort :cleanup :subset :forged-frame-count :build))
  (let ((r (copy-tree *native*)))
    (case case
      (:recorder-exit (setf (getf (getf r :finalization) :client-exit) 1))
      (:abort (setf (getf r :abort) (copy-tree (getf (getf r :finalization) :result))))
      (:cleanup (setf (getf r :cleanup) :fail))
      (:subset (setf (getf r :checks) (butlast (getf r :checks))))
      (:forged-frame-count (setf (getf (getf (getf r :stages) :recorder) :frame-verified) 14))
      (:build (setf (getf r :build) :fail)))
    (check (list :native-reject case) (refuse-p (lambda () (vttest-screen-native-contract:validate-native r "x11"))))
    (multiple-value-bind (observations evidence)
        (vttest-screen-driver:summarize-native (vttest-screen-driver:make-manifest "x11") r 0)
      (check (list :summary-reject case) (getf evidence :native-failed))
      (check (list :aggregation-reject case)
             (not (eq :pass (vttest-area:aggregate-observations
                            (getf (vttest-screen-driver:make-manifest "x11") :scenarios) observations)))))))
;; Refuse duplicate publication through the actual exclusive writer.
(check :native-publication-failure (refuse-p #'vttest-screen-native-contract:publish-native))
(check :result-publication-failure
       (refuse-p (lambda () (vttest-area::write-result *fixture-root* *outer*))))
;; Compare one existing original image through the actual adapter and decoder.
;; This is historical-input integration, not proof of a newly run backend.
(let* ((root (namestring (merge-pathnames "historical-adapter/" *attempt*)))
       (stem "recorder/pause-01-") (values nil))
  (ensure-directories-exist (concatenate 'string root "recorder/.reserve"))
  (dolist (suffix '("before.sexp" "ready.sexp" "pre.sexp" "final.sexp" "native.sexp" "request.sexp"))
    (let ((value (vttest-screen-native-contract:read-data (merge-pathnames (concatenate 'string "pause-01-" suffix) *historical*))))
      (fixture-file root (concatenate 'string stem suffix) value) (push value values)))
  (setf values (nreverse values))
  (link-original (merge-pathnames "pause-01-actual.png" *historical*) (concatenate 'string root stem "actual.png"))
  (let* ((request (sixth values))
         (ref (vttest-screen-pixels:compare-capture root 1 (vttest-screen-oracle:screen-snapshot 1)
                (first values) (second values) (third values) (fourth values) (fifth values)
                (getf request :target) (getf request :ack-output) (getf request :surface-id)))
         (c (list :pause 1 :pixels :pass :frame :pass :native (fifth values) :comparison ref)))
    (check :historical-real-adapter (vttest-screen-native-contract::validate-comparison c root))
    (let* ((rgba (vttest-screen-pixels::octets (concatenate 'string root stem "actual.rgba")))
           (font (vttest-screen-pixels::octets vttest-screen-pixels::+font+)))
      (setf (aref rgba 0) (logxor 1 (aref rgba 0)))
      (check :changed-pixel-is-fail
        (not (getf (vttest-screen-oracle:compare-screen-snapshot
                    (vttest-screen-oracle:screen-snapshot 1) rgba font :blink-phase t) :pass))))
    ;; Rebind only inert fixture files; retained original files are never changed.
    (dolist (case '(:png :dimensions :phase :decode-missing :decode-fail :comparison-missing :comparison-fail))
      (let* ((child (namestring (merge-pathnames (format nil "comparison-~(~a~)/" case) *attempt*)))
             (r (copy-tree (vttest-screen-native-contract:read-data (concatenate 'string root stem "comparison.sexp")))))
        (ensure-directories-exist (concatenate 'string child "recorder/.reserve"))
        (dolist (suffix '("before.sexp" "ready.sexp" "pre.sexp" "final.sexp" "native.sexp" "request.sexp"
                          "actual.png" "actual.rgba" "identify.stdout" "identify.stderr" "identify.exit"
                          "decode.stdout" "decode.stderr"))
          (link-original (pathname (concatenate 'string root stem suffix)) (concatenate 'string child stem suffix)))
        (fixture-file child (concatenate 'string stem "identify.argv.sexp")
          (list vttest-screen-pixels::+decoder+ "identify" "-format" "%w %h" (concatenate 'string child stem "actual.png")))
        (fixture-file child (concatenate 'string stem "decode.argv.sexp")
          (list vttest-screen-pixels::+decoder+ (concatenate 'string child stem "actual.png") "-depth" "8"
                (concatenate 'string "rgba:" child stem "actual.rgba")))
        (fixture-file child (concatenate 'string stem "decode.exit") (if (eq case :decode-fail) 1 0))
        (loop for key in '(:identify :decode) for action in '("identify" "decode") do
          (setf (getf r key) (loop for suffix in '(".argv.sexp" ".stdout" ".stderr" ".exit") collect
             (vttest-screen-native-contract:reference child (concatenate 'string stem action suffix)))))
        (case case
          (:png (setf (getf (getf r :png) :sha256) (make-string 64 :initial-element #\0)))
          (:dimensions (setf (getf r :width) 639))
          (:phase (setf (getf r :phase) nil))
          (:decode-missing (setf (getf r :decode) nil))
          (:comparison-missing (remf r :comparison))
          (:comparison-fail (setf (getf (getf r :comparison) :different-pixels) 1)))
        (fixture-file child (concatenate 'string stem "comparison.sexp") r)
        (let ((bad (copy-tree c)))
          (setf (getf bad :comparison) (vttest-screen-native-contract:reference child (concatenate 'string stem "comparison.sexp")))
          (check (list :comparison-refusal case)
                 (refuse-p (lambda () (vttest-screen-native-contract::validate-comparison bad child)))))))))
;; Late actual exit and late strict import are rejected by the real native reader.
(dolist (case '(:late-exit :late-import))
  (let* ((root (namestring (merge-pathnames (format nil "clock-~(~a~)/" case) *attempt*)))
         (r (copy-tree *native*)))
    (ensure-directories-exist (concatenate 'string root "native/.reserve"))
    (ensure-directories-exist (concatenate 'string root "recorder/.reserve"))
    (dolist (relative '("native/stages.sexp" "recorder/RESULT.sexp" "native/client.exit"))
      (link-original (pathname (concatenate 'string *fixture-root* relative)) (concatenate 'string root relative)))
    (fixture-file root "native/finalization-clock.sexp"
      (list :clock :clock-monotonic :limit-seconds 15 :start-ns 100
            :end-ns (if (eq case :late-exit) 15000000101 200)
            :state :exited :pid 1 :token "fixture" :run root :backend "x11"))
    (fixture-file root "native/import-deadline.sexp"
      (list :clock :clock-monotonic :deadline-ns 15000000100
            :validated-ns (if (eq case :late-exit) 15000000102 15000000100) :state :valid))
    (check (list :clock-refusal case)
           (refuse-p (lambda () (vttest-screen-native-contract:validate-native r "x11" root))))))
;; A non-screen dispatch cannot accidentally select the screen adapter.
(check :non-screen-dispatch-boundary
       (refuse-p (lambda () (vttest-area:run-selected
                            '(:area "keyboard" :native-driver "screen-x11-v1"
                              :scenarios ((:recorder-label "keyboard-cursor"))) nil))))
(dolist (file *sources*)
  (check (list :final-source-equality file)
         (equalp (bytes (merge-pathnames (concatenate 'string "tests/" file) *workspace*))
                 (bytes (merge-pathnames (concatenate 'string "current/" file) *attempt*)))))
(defun shell-zero-controls ()
  "Exercise the real cleanup/exit selector with inert lifecycle fixtures."
  (dolist (case '(:zero-pass :three-pass :zero-unverified :recorder-nonzero :incomplete
                  :subset :cleanup :abort :publish-error :publish-fail :publish-missing))
    (let* ((name (string-downcase (symbol-name case)))
           (root (namestring (merge-pathnames (concatenate 'string "shell-" name "/") *attempt*)))
           (script (concatenate 'string root "control.sh"))
           (source (namestring (merge-pathnames "current/vttest-screen-native.sh" *attempt*)))
           (bash "/nix/store/kwyi4bq5s2qgwbhz6cs4gjy57c2zdxws-bash-interactive-5.3p20/bin/bash")
           (expected (if (member case '(:zero-pass :three-pass)) 0 1)))
      (ensure-directories-exist (concatenate 'string root "native/.reserve"))
      (ensure-directories-exist (concatenate 'string root "recorder/.reserve"))
      (put-text script
        (format nil "set -Eeuo pipefail~%source ~s~%run=~s~%native=$run/native; recorder=$run/recorder~%client=; binary=; runtime=; reason=fixture~%endpoints=(); started=T; cleanup=:pass; build_status=:pass~%attempted=15; reached=15; captured=~d; client_exit=~d; finalization=~a~%printf '%s\\n' '~a' > \"$native/import.stdout\"~%screen_finalize() { return 0; }~%screen_stop() { return ~d; }~%screen_contract() {~% [[ $1 == publish ]] || return 91~% ~a~%}~%~a~%trap screen_cleanup EXIT~%exit ~d~%"
                source root (if (eq case :subset) 14 15) (if (eq case :recorder-nonzero) 1 0)
                (if (eq case :incomplete) ":invalid-result" ":complete")
                (if (eq case :zero-unverified) ":UNVERIFIED" ":PASS")
                (if (eq case :cleanup) 1 0)
                (case case
                  (:publish-error "return 1")
                  (:publish-fail "printf ':FAIL\\n' > \"$native/publish.stdout\"; return 0")
                  (:publish-missing "return 0")
                  (t "printf ':PASS\\n' > \"$native/publish.stdout\"; printf '(:fixture T)\\n' > \"$run/SCREEN-RUN.sexp\"; return 0"))
                (if (eq case :abort) ": > \"$recorder/ABORT\"" ":")
                (if (eq case :three-pass) 3 0)))
      (put (concatenate 'string root "command.sexp") (list bash "--noprofile" "--norc" script))
      (let* ((p (sb-ext:run-program bash (list "--noprofile" "--norc" script) :input nil
                   :output (concatenate 'string root "stdout") :error (concatenate 'string root "stderr") :wait t))
             (exit (sb-ext:process-exit-code p)))
        (put (concatenate 'string root "exit") exit)
        (check (list :shell-zero case) (eql expected exit))
        (check (list :shell-stream case) (zerop (length (bytes (concatenate 'string root "stderr")))))))))
(shell-zero-controls)
(put (merge-pathnames "checks.sexp" *attempt*) (reverse *checks*))
(put (merge-pathnames "fixture-root.sexp" *attempt*) *fixture-root*)
(format t "Visible-screen acceptance controls passed: ~d assertions.~%" (length *checks*))
