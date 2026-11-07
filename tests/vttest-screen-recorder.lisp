;;;; One disposable native session. Test-tree instrumentation only.
(require :sb-posix)
(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system "lispterm")
(load (merge-pathnames "vttest-screen-frame.lisp" *load-truename*))
(load (merge-pathnames "vttest-screen-oracle.lisp" *load-truename*))
(load (merge-pathnames "vttest-screen-native-contract.lisp" *load-truename*))
(load (merge-pathnames "vttest-screen-pixels.lisp" *load-truename*))
(defpackage #:vttest-screen-recorder (:use #:cl))
(in-package #:vttest-screen-recorder)
(defparameter *root* (uiop:ensure-directory-pathname (uiop:getenv "LISPTERM_VTTEST_RUN_ROOT")))
(defparameter *art* (merge-pathnames "recorder/" *root*))
(defparameter *backend* (ecase (intern (string-upcase (uiop:getenv "LISPTERM_VTTEST_BACKEND")) :keyword)
                          (:x11 :x11) (:wayland :wayland)))
(defparameter *binary* (uiop:getenv "LISPTERM_VTTEST_EXECUTABLE"))
(defparameter *lock* (sb-thread:make-mutex :name "screen recorder"))
(defparameter *output* 0)
(defparameter *input* 0)
(defparameter *sequence* 0)
(defparameter *pause* 0)
(defparameter *events* nil)
(defparameter *sources* (make-hash-table :test #'eq))
(defvar *write-source* nil)
(defparameter *committed* -1)
(defparameter *ack* -1)
(defparameter *surface* nil)
(defparameter *presentation-witness* nil)

(defparameter *native-failure-seen* nil)
(define-condition native-abort (error)
  ((reason :initarg :reason :reader native-abort-reason))
  (:report (lambda (condition stream) (write-string (native-abort-reason condition) stream))))
(defun check-native-failure ()
  "Validate on the controlling thread, before accepting any simultaneous success."
  (let ((file (merge-pathnames "native/native-failure.sexp" *root*)))
    (when (probe-file file)
      (let ((reason
              (handler-case
                  (progn
                    (when *native-failure-seen* (error "Duplicate native failure"))
                    (setf *native-failure-seen* t)
                    (let ((record (vttest-screen-native-contract:read-data file)))
                      (vttest-screen-native-contract::validate-native-failure
                        record (namestring *root*) (string-downcase *backend*) *pause* t)
                      (save "native-failure-observed.sexp"
                        (list :origin (vttest-screen-native-contract:reference
                                        (namestring *root*) "native/native-failure.sexp")
                              :record record))
                      (format nil "Native ~a at ~a pause ~d; origin ~s"
                              (getf record :reason) (getf record :stage) *pause* (getf record :origin))))
                (error (c) (format nil "Native failure protocol refusal: ~a" c)))))
        (error 'native-abort :reason reason)))))

;;; Diagnostic observations are independent of the RESULT schema and never gate a capture.
(defparameter *observations* nil)
(defparameter *observation-seen* (make-hash-table :test #'equal))
(defparameter *saved-bindings* nil)
(defparameter *observation-active* nil)
(defparameter *owned-child* :unknown)
(defparameter *primary-nodes* (make-hash-table :test #'eq))
(defvar *primary-write* nil)
(defvar *wait-observer* nil)

(defun observe-once (key kind &rest details)
  "Record at most one sample per explicit key; ranges are recorder counters, not query times."
  (sb-thread:with-mutex (*lock*)
    (unless (gethash key *observation-seen*)
      (setf (gethash key *observation-seen*) t)
      (push (append (list :kind kind :key key :ticks (get-internal-real-time)
                          :event *sequence* :output *output* :input *input*
                          :pause *pause* :child-pid *owned-child*) details)
            *observations*))))

(defun call-observed (key function arguments &optional after)
  "Preserve all values and every original condition/nonlocal exit. No handler intercepts it."
  (observe-once (list key :entry) :boundary :boundary key :edge :entry)
  (let ((returned nil))
    (unwind-protect
         (multiple-value-prog1 (apply function arguments)
           (when after (funcall after arguments))
           (setf returned t))
      (observe-once (list key :return) :boundary :boundary key
                    :edge (if returned :return :nonlocal-exit)))))

(defun restore-recorder-bindings ()
  (dolist (entry *saved-bindings*)
    (setf (symbol-function (car entry)) (cdr entry)))
  (setf *saved-bindings* nil *observation-active* nil))

(defun install-startup-observers ()
  ;; Snapshot ALL bindings, including the older wire/presentation wrappers.
  (when *observation-active* (error "Recorder wrappers already installed"))
  (setf *observation-active* t)
  (dolist (symbol '(lispterm:open lispterm:close lispterm.pty:spawn-on-pty
                    lispterm::start-window-worker lispterm::window-worker lispterm::window-open
                    lispterm.pty:pty-read lispterm.pty:pty-write
                    lispterm::session-write lispterm::session-flush-input
                    lispterm.display.wayland::protocol-present lispterm.display.wayland::protocol-pump))
    (if (fboundp symbol)
        (push (cons symbol (symbol-function symbol)) *saved-bindings*)
        (observe-once (list :unavailable symbol) :unknown :boundary symbol :reason :unbound)))
  (dolist (spec '((lispterm:open :recorder-open)
                  (lispterm:close :public-close)
                  (lispterm.pty:spawn-on-pty :child-spawn)
                  (lispterm::start-window-worker :worker-start)
                  (lispterm::window-worker :worker)
                  (lispterm::window-open :window-open)))
    (destructuring-bind (symbol key) spec
      (when (fboundp symbol)
        (let ((original (symbol-function symbol)) (boundary key))
          (setf (symbol-function symbol)
                (lambda (&rest arguments)
                  (call-observed boundary original arguments
                    (when (eq boundary :child-spawn)
                      (lambda (args)
                        (let ((process (lispterm.pty:pty-process (first args))))
                          (setf *owned-child* (if process (lispterm.pty:process-pid process) :unknown)))))))))))))

(defun save-observations ()
  (save "observations.sexp"
    (list :kind :recorder-observations :version 1 :run (namestring *root*) :backend *backend*
          :clock :get-internal-real-time :units-per-second internal-time-units-per-second
          :child-pid *owned-child* :association :single-owned-recorder-session
          :query-production-time :unknown :sampling :sequential-not-atomic
          :missing (loop for key in '((:recorder-open :entry) (:recorder-open :return)
                                      (:child-spawn :entry) (:child-spawn :return)
                                      (:worker-start :entry) (:worker :entry)
                                      (:window-open :entry) (:window-open :return)
                                      (:first-pty-read :entry) (:first-pty-read :return)
                                      :first-output :primary-enqueue :primary-accepted)
                         unless (gethash key *observation-seen*)
                           collect (list :key key :status :unknown :reason :not-observed))
          :samples (reverse *observations*))))

(defun predicate-details (expected actual expected-cursor actual-cursor content status n stage)
  "Explain every hold conjunct, without changing the original predicate. Exact grids retained."
  (list :stage stage :expected-grid expected :actual-grid actual :grid-match (string= expected actual)
        :expected-cursor expected-cursor :actual-cursor actual-cursor
        :cursor-match (and (= (getf expected-cursor :row) (getf actual-cursor :row))
                           (= (getf expected-cursor :column) (getf actual-cursor :column)))
        :expected-grid-size (list 24 (if (member n '(3 5)) 132 80))
        :actual-grid-size (list (getf status :rows) (getf status :columns))
        :actual-pty-size (list (getf status :pty-rows) (getf status :pty-columns))
        :geometry (copy-tree (getf status :geometry))
        :settled (vttest-screen-frame:settled-p status)
        :expected-blink-content content :actual-blink-content (getf status :blink-visible)
        :blink-content-match (eq content (getf status :blink-visible))
        :output *output* :status-output (getf status :bytes-received)
        :output-match (eql *output* (getf status :bytes-received))
        :input-pending (getf status :input-pending) :input-empty (eql 0 (getf status :input-pending))
        :ack-output *ack* :presentation-witness (copy-tree *presentation-witness*)
        :frame-complete (or (eq *backend* :x11) (completed-wayland-frame-p status *output*))
        :blink-phase (getf status :blink-phase) :blink-on-required (= n 14)
        :blink-phase-match (or (/= n 14) (eq t (getf status :blink-phase)))))

(defun await-target (host snapshot n stage)
  ;; Each saved component comes from the SAME candidate evaluation that supplied
  ;; the predicate result. Status, grid and cursor reads are sequential, not atomic.
  (let ((candidate nil) (text (expected-text snapshot)))
    (let ((*wait-observer*
            (lambda (edge)
              (apply #'observe-once (list :predicate n stage edge) :predicate
                     :edge edge candidate))))
      (await host
        (lambda ()
          (let* ((status (lispterm:status host)) (actual (grid host))
                 (session (lispterm::host-session host))
                 (cursor (sb-thread:with-mutex ((lispterm::session-lock session))
                           (let ((screen (lispterm::session-screen session)))
                             (list :row (1+ (lispterm.engine:cursor-row screen))
                                   :column (1+ (lispterm.engine:cursor-column screen))))))
                 (details (predicate-details text actual
                            (getf (vttest-screen-oracle:snapshot-metadata snapshot) :cursor) cursor
                            (getf (vttest-screen-oracle:snapshot-metadata snapshot) :blink-visible)
                            status n stage))
                 (matches (if (eq stage :grid-wait) (getf details :grid-match)
                              (every (lambda (key) (getf details key))
                                     '(:grid-match :cursor-match :settled :output-match :input-empty
                                       :blink-content-match :frame-complete :blink-phase-match)))))
            (setf candidate (append (list :candidate-result (not (null matches))
                                         :candidate-ticks (get-internal-real-time)
                                         :candidate-event *sequence* :candidate-output *output*
                                         :candidate-input *input*) details))
            (when matches (if (eq stage :grid-wait) t status))))))))

(defun path (name) (merge-pathnames name *art*))
(defun save (name value)
  (with-open-file (s (path name) :direction :output :if-exists :error)
    (let ((*print-pretty* nil) (*print-length* nil) (*print-level* nil))
      (write value :stream s) (terpri s))))

(defun publish-request (name writer)
  "Publish closed request bytes once, without replacing any final pathname."
  (let* ((final (path name))
         (temporary (pathname (concatenate 'string (namestring final) ".pending")))
         (owned nil))
    (unwind-protect
         (progn
           (with-open-file (s temporary :direction :output :if-exists :error
                                       :if-does-not-exist :create)
             (setf owned t)
             (funcall writer s))
           ;; LINK is atomic and refuses an existing destination, including a symlink.
           (sb-posix:link (namestring temporary) (namestring final)))
      ;; Never remove a preexisting temporary or replace the writer's condition.
      (when owned (ignore-errors (delete-file temporary))))))

(defun publish-request-value (name value)
  (publish-request name
    (lambda (s)
      (let ((*print-pretty* nil) (*print-length* nil) (*print-level* nil))
        (write value :stream s) (terpri s)))))
(defun read-value (name)
  (with-open-file (s (path name))
    (let ((*read-eval* nil))
      (let ((value (read s))) (unless (eq (read s nil :eof) :eof) (error "Extra receipt form")) value))))
(defun touch (name) (with-open-file (s (path name) :direction :output :if-exists :error)))
(defun event-unlocked (kind &rest data)
  (let ((record (append (list :seq (incf *sequence*) :kind kind :pause *pause*
                              :time (get-internal-real-time) :out *output* :in *input*) data)))
    (push record *events*)
    (with-open-file (s (path "events.sexp") :direction :output :if-exists :append :if-does-not-exist :create)
      (let ((*print-pretty* nil) (*print-length* nil) (*print-level* nil))
        (write record :stream s) (terpri s)))
    record))
(defun event (kind &rest data) (sb-thread:with-mutex (*lock*) (apply #'event-unlocked kind data)))
(defun wire (kind bytes &optional source)
  (when (plusp (length bytes))
    (sb-thread:with-mutex (*lock*)
      (let* ((outp (eq kind :wire-out)) (start (if outp *output* *input*))
             (end (+ start (length bytes))))
        (with-open-file (s (path (if outp "output.raw" "input.raw")) :direction :output
                           :element-type '(unsigned-byte 8) :if-exists :append :if-does-not-exist :create)
          (write-sequence bytes s))
        (event-unlocked kind :start start :end end :bytes (copy-seq bytes) :source source)
        (if outp (setf *output* end) (setf *input* end))
        (values *sequence* start end)))))

(defun install-wire-recorder ()
  (let ((reader (symbol-function 'lispterm.pty:pty-read))
        (writer (symbol-function 'lispterm.pty:pty-write))
        (enqueue (symbol-function 'lispterm::session-write))
        (flush (symbol-function 'lispterm::session-flush-input)))
    (setf (symbol-function 'lispterm.pty:pty-read)
          (lambda (&rest args)
            (let ((entry (get-internal-real-time)))
              (call-observed :first-pty-read
                (lambda ()
                  (multiple-value-call
                      (lambda (bytes &rest more)
                        (let ((returned (get-internal-real-time)))
                          (when (and (vectorp bytes) (plusp (length bytes)))
                            (multiple-value-bind (seq start end) (wire :wire-out bytes)
                              (observe-once :first-output :first-pty-output
                                            :read-entry-ticks entry :read-return-ticks returned
                                            :event-range (list seq seq) :output-range (list start end)))))
                        (values-list (cons bytes more)))
                    (apply reader args))) nil)))
          (symbol-function 'lispterm.pty:pty-write)
          (lambda (pty bytes &rest args)
            (let ((entry (get-internal-real-time)))
              (multiple-value-call
                  (lambda (n &rest more)
                    (let ((returned (get-internal-real-time)))
                      (when (plusp n)
                        (multiple-value-bind (seq start end) (wire :wire-in (subseq bytes 0 n) *write-source*)
                          (when *primary-write*
                            (observe-once :primary-accepted :primary-reply-accepted
                                          :write-entry-ticks entry :write-return-ticks returned
                                          :offered (length bytes) :accepted n
                                          :event-range (list seq seq) :input-range (list start end))))))
                    (values-list (cons n more)))
                (apply writer pty bytes args))))
          (symbol-function 'lispterm::session-write)
          (lambda (session bytes)
            (let* ((entry (get-internal-real-time))
                   (n (funcall enqueue session bytes)) (returned (get-internal-real-time)))
              (when (plusp n)
                (let ((origin (if lispterm::*enqueueing-response* :reply :driver)))
                  (setf (gethash (car (lispterm::session-input-tail session)) *sources*) origin)
                  (let ((record (event :enqueue :source origin :bytes (copy-seq bytes))))
                    (when (and (eq origin :reply) (equalp bytes #(27 91 63 49 59 50 99)))
                      (setf (gethash (car (lispterm::session-input-tail session)) *primary-nodes*) t)
                      (observe-once :primary-enqueue :primary-reply-enqueue
                                    :enqueue-entry-ticks entry :enqueue-return-ticks returned
                                    :event-range (list (getf record :seq) (getf record :seq)) :length n))))) n))
          (symbol-function 'lispterm::session-flush-input)
          (lambda (session)
            (let ((*write-source* (gethash (car (lispterm::session-input-head session)) *sources*))
                  (*primary-write* (gethash (car (lispterm::session-input-head session)) *primary-nodes*)))
              (funcall flush session))))))

(defun install-presentation-recorder ()
  (let ((present (symbol-function 'lispterm.display.wayland::protocol-present))
        (pump (symbol-function 'lispterm.display.wayland::protocol-pump)))
    (setf (symbol-function 'lispterm.display.wayland::protocol-present)
          (lambda (state &rest args)
            (let ((result (apply present state args)))
              (when result
                (setf *committed* *output*
                      *surface* (lispterm.display.wayland::wayland-state-surface state))
                (event :wayland-present :surface *surface* :output *committed*
                       :commit-generation (lispterm.display.wayland::wayland-state-commit-generation state))) result))
          (symbol-function 'lispterm.display.wayland::protocol-pump)
          (lambda (state &rest args)
            (prog1 (apply pump state args)
              (when (and (not (lispterm.display.wayland::wayland-state-frame-pending-p state))
                         (> *committed* -1))
                (let ((witness (list :surface (lispterm.display.wayland::wayland-state-surface state)
                                     :toplevel (lispterm.display.wayland::wayland-state-toplevel state)
                                     :commit-generation (lispterm.display.wayland::wayland-state-commit-generation state)
                                     :frame-generation (lispterm.display.wayland::wayland-state-frame-generation state)
                                     :pending nil :output *committed*)))
                  (unless (equal witness *presentation-witness*)
                    (setf *ack* *committed* *presentation-witness* witness)
                    (event :wayland-frame-complete :witness witness)))))))))

(defun grid (host)
  (let ((session (lispterm::host-session host)))
    (sb-thread:with-mutex ((lispterm::session-lock session))
      (let ((screen (lispterm::session-screen session)))
        (with-output-to-string (s)
          (dotimes (r (lispterm.engine:rows screen))
            (dotimes (c (lispterm.engine:columns screen))
              (write-char (or (lispterm.engine:cell-character (lispterm.engine:cell-at screen r c)) #\Space) s))
            (terpri s)))))))
(defun expected-text (snapshot)
  (with-output-to-string (s)
    (loop for cell across (vttest-screen-oracle:snapshot-cells snapshot) for i from 1 do
      (write-char (code-char (vttest-screen-oracle:cell-code cell)) s)
      (when (zerop (mod i (vttest-screen-oracle:snapshot-columns snapshot))) (terpri s)))))
(defun cursor-matches-p (host snapshot)
  (let* ((expected (getf (vttest-screen-oracle:snapshot-metadata snapshot) :cursor))
         (session (lispterm::host-session host)))
    (sb-thread:with-mutex ((lispterm::session-lock session))
      (let ((screen (lispterm::session-screen session)))
        (and (= (getf expected :row) (1+ (lispterm.engine:cursor-row screen)))
             (= (getf expected :column) (1+ (lispterm.engine:cursor-column screen))))))))
(defun completed-wayland-frame-p (status target)
  (let ((g (getf status :geometry)))
    (and (>= *ack* target) *presentation-witness*
         (null (getf g :frame-pending-p))
         (eql (getf *presentation-witness* :commit-generation) (getf g :commit-generation))
         (eql (getf *presentation-witness* :frame-generation) (getf g :frame-generation))
         (eql (getf g :commit-generation) (getf g :frame-generation)))))
(defun check-host (host)
  (when (lispterm:host-errors host) (error "Host errors: ~s" (lispterm:host-errors host))))
(defun await (host predicate &optional (seconds 20))
  (loop with end = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
        with first = t
        for value = (progn (check-native-failure) (funcall predicate))
        do (when (and first *wait-observer*) (funcall *wait-observer* :first-candidate))
           (setf first nil)
           (check-native-failure)
           (when host (check-host host))
        when value return value
        when (> (get-internal-real-time) end)
          do (when *wait-observer* (funcall *wait-observer* :final-failed-wait))
             (error "Deadline at pause ~d" *pause*)
        do (sleep .005)))
(defun send (host text) (event :driver :bytes (map 'vector #'char-code text)) (lispterm:send-input host text))
(defun pause-name (pause suffix) (format nil "pause-~2,'0d-~a" pause suffix))

(defun capture-hold (host n snapshot)
  (let* ((text (expected-text snapshot))
         (content (getf (vttest-screen-oracle:snapshot-metadata snapshot) :blink-visible))
         (before (await-target host snapshot n :capture-hold))
         (target (getf before :bytes-received)) (ready nil) (pre nil) (final nil) (native nil))
    (save (pause-name n "grid.sexp") (grid host))
    (save (pause-name n "before.sexp") before)
    (event :source-endpoint :status :unverified :reason :full-session-wire-not-yet-admitted)
    (lispterm:invalidate host)
    (setf ready (await host (lambda ()
                             (let ((st (lispterm:status host)))
                               (when (> (getf st :frames) (1+ (getf before :frames)))
                                 (error "More than one fresh frame"))
                               (and (= (getf st :frames) (1+ (getf before :frames)))
                                    (or (eq *backend* :x11) (completed-wayland-frame-p st target)) st)))))
    (save (pause-name n "ready.sexp") ready)
    (save (pause-name n "presentation.sexp") (list :ack-output *ack* :witness *presentation-witness*))
    (unless (and (vttest-screen-frame:settled-p ready)
                 (eql target (getf ready :bytes-received)) (= target *output*)
                 (eq content (getf ready :blink-visible))
                 (or (eq (getf ready :blink-phase) nil) (eq (getf ready :blink-phase) t))
                 (eq (getf before :blink-phase) (getf ready :blink-phase))
                 (eql 0 (getf ready :input-pending))
                 (string= text (grid host)) (cursor-matches-p host snapshot))
      (error "Fresh READY target/phase/content/settlement refusal"))
    (let* ((g (getf ready :geometry))
           (window (if (eq *backend* :x11) (getf g :window-id) *surface*)))
      (publish-request-value (pause-name n "request.sexp")
            (list :pause n :backend *backend* :status ready :target target :ack-output *ack*
                  :surface-id *surface* :executable *binary*))
      (publish-request (pause-name n "request")
        (lambda (s)
          (format s "~d ~d ~d ~d 1 ~d ~d~%" n window (getf g :actual-width) (getf g :actual-height) target *ack*)))
      (await host (lambda () (probe-file (path (pause-name n "native-ready.sexp")))))
      (let ((receipt (read-value (pause-name n "native-ready.sexp"))))
        (unless (and (eql 3 (getf receipt :version)) (eq *backend* (getf receipt :backend))
                     (eql n (getf receipt :pause)) (eq t (getf receipt :owned))
                     (eql 0 (getf receipt :capture-count)) (eql 1 (getf receipt :scale))
                     (eql (getf g :actual-width) (getf receipt :width))
                     (eql (getf g :actual-height) (getf receipt :height))
                     (eql window (getf receipt (if (eq *backend* :x11) :window-id :surface-id))))
          (error "Native READY identity/geometry refusal")))
      (setf pre (lispterm:status host))
      (save (pause-name n "pre.sexp") pre)
      (unless (and (equal ready pre) (= target *output*) (string= text (grid host))
                   (cursor-matches-p host snapshot)
                   (or (eq *backend* :x11) (completed-wayland-frame-p pre target)))
        (error "READY/PRE or target changed before authorization"))
      (touch (pause-name n "go"))
      (await host (lambda () (probe-file (path (pause-name n "done")))))
      (setf final (lispterm:status host))
      (save (pause-name n "final.sexp") final)
      (setf native (read-value (pause-name n "native.sexp"))))
    (vttest-screen-frame:validate-frame before ready pre final native
      :pause n :target target :blink-content content :endpoint nil :ack-output *ack* :surface-id *surface*)
    (unless (and (= target *output*) (string= text (grid host)) (cursor-matches-p host snapshot)
                 (or (eq *backend* :x11) (completed-wayland-frame-p final target)))
      (error "Final source target changed"))
    (let ((comparison (vttest-screen-pixels:compare-capture
                       (namestring *root*) n snapshot before ready pre final native target *ack* *surface*)))
      (list :pause n :pixels :pass :bytes :unverified :timing :pass
            :frame :pass :target :recognized-navigation-only :native native :comparison comparison
            :evidence (loop for suffix in '("before.sexp" "ready.sexp" "pre.sexp" "final.sexp" "native.sexp" "presentation.sexp")
                            collect (pause-name n suffix))))))

(defun raw (name)
  (with-open-file (s (path name) :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence v s) v)))

(defun record-launch-identity (host)
  (let* ((status (lispterm:status host)) (pid (getf status :child-pid))
         (command (list "/nix/store/cb987zkxvsv0490753ilrvn5yk7ylgxi-coreutils-9.12/bin/readlink"
                        "-f" (format nil "/proc/~d/exe" pid))))
    (event :owned-proc-exe-command :argv command)
    (multiple-value-bind (out err code)
        (uiop:run-program command :output :string :error-output :string :ignore-error-status t)
      (save "launch.sexp" (list :argv (list *binary* "-l" (namestring (path "application.log")) "24x80.132")
                                :requested-binary *binary* :canonical-binary (namestring (truename *binary*))
                                :child-pid pid :host status :proc-exe-command command
                                :proc-exe-stdout out :proc-exe-stderr err :proc-exe-exit code))
      (unless (and (eql code 0) (string= (namestring (truename *binary*))
                                        (string-right-trim '(#\Newline) out)))
        (error "Launched child executable identity mismatch or unobserved")))))
(defun run-main ()
  (when (probe-file *art*) (error "Recorder directory already exists"))
  (ensure-directories-exist (path ".reserve"))
  (unless (and (uiop:absolute-pathname-p *binary*) (probe-file *binary*)) (error "Missing built executable"))
  (install-startup-observers)
  (install-wire-recorder) (install-presentation-recorder)
  (let ((host nil) (checks nil) (failure nil) (attempted 0) (reached 0) (captured 0)
        (released 0) (cleanup nil) (native-aborted nil) (accounting :unverified))
    (unwind-protect
         (handler-case
             (progn
               (setf host (lispterm:open :backend *backend* :rows 24 :columns 80
                                        :title "Lispterm-screen-owned-run"
                                        :argv (list *binary* "-l" (namestring (path "application.log")) "24x80.132")))
               (record-launch-identity host)
               ;; Native observes initial placement before any target or BEFORE transaction.
               (publish-request-value "placement-request.sexp" (list :pid (sb-posix:getpid)))
               (await host (lambda () (probe-file (path "placement-ready.sexp"))))
               (await host (lambda () (search "Enter choice number" (grid host))))
               (send host (format nil "2~c" #\Return))
               (loop for n from 1 to 15 do
                 (setf *pause* n attempted n)
                (let ((snapshot (vttest-screen-oracle:screen-snapshot n :columns (if (member n '(3 5)) 132 80))))
                  (await-target host snapshot n :grid-wait)
                   (incf reached)
                   (push (capture-hold host n snapshot) checks)
                   (incf captured)
                   (send host (string #\Return)) (incf released)))
               (setf *pause* 0)
               (await host (lambda () (search "Enter choice number" (grid host))))
               (send host (format nil "0~c" #\Return))
               (await host (lambda () (eq :exited (getf (lispterm:status host) :process-state))))
               (setf cleanup :graceful-exit))
           (error (c)
             (setf native-aborted (typep c 'native-abort))
             (setf failure (princ-to-string c))
             (save "abort.sexp" (list :pause *pause* :reason failure))
             (touch "ABORT")
             (when (plusp *pause*)
               (push (list :pause *pause* :pixels :unverified :bytes :unverified :timing :fail
                           :frame :fail :reason failure :evidence (pause-name *pause* "")) checks))))
      ;; After failure there are no more captures or target PASS claims.
      (when host
        (when native-aborted (setf cleanup :cooperative-native-abort))
        (when (and failure (not native-aborted))
          (handler-case
              (progn
                (when (plusp reached)
                  (loop for n from (1+ released) to 15 do
                    (send host (string #\Return)) (sleep .1)))
                (await host (lambda () (search "Enter choice number" (grid host))) 5)
                (send host (format nil "0~c" #\Return))
                (await host (lambda () (eq :exited (getf (lispterm:status host) :process-state))) 5)
                (setf cleanup :graceful-exit-after-failure))
            (error (c) (setf cleanup (list :graceful-exit-unverified (princ-to-string c))))))
        (save "terminal-status.sexp" (lispterm:status host))
        (handler-case (lispterm:close host)
          (error (c) (setf failure (or failure (princ-to-string c))
                           cleanup (list cleanup :close-error (princ-to-string c)))))))
    (handler-case
        (when (and (probe-file (path "output.raw")) (probe-file (path "input.raw")))
          (vttest-screen-frame:validate-events (reverse *events*) (raw "output.raw") (raw "input.raw"))
          (setf accounting :pass))
      (error (c) (setf accounting (list :fail (princ-to-string c)) failure (or failure "Wire accounting failure"))))
    (unless (or failure
                (and (= attempted 15) (= reached 15) (= captured 15)
                     (eq cleanup :graceful-exit)
                     (= (length checks) 15)
                     (every (lambda (c) (every (lambda (k) (eq :pass (getf c k)))
                                                '(:pixels :timing :frame))) checks)))
      (setf failure "Incomplete visible-screen run"))
    (let ((result (list :kind :vttest-screen-run :version 2 :backend (string-downcase *backend*)
                        :status (if failure :fail :pass) :attempted attempted :reached reached
                        :captured (loop for n from 1 to attempted count (probe-file (path (pause-name n "actual.png"))))
                        :frame-verified captured :bytes :unverified :wire-accounting accounting
                        :reason (or failure "Fifteen strict visible-screen comparisons passed")
                        :cleanup cleanup :checks
                        (loop for n from 1 to 15 collect
                          (or (find n checks :key (lambda (v) (getf v :pause)))
                              (list :pause n :pixels :unverified :bytes :unverified :timing :unverified)))
                        :evidence '("events.sexp" "output.raw" "input.raw" "application.log" "launch.sexp" "terminal-status.sexp"))))
      (save "RESULT.sexp" result) (save "end.sexp" (list :status (getf result :status)))
      (touch "END")
      (if failure 1 0))))

(defun main ()
  (let ((code nil))
    (unwind-protect
         (setf code (run-main))
      (restore-recorder-bindings)
      (when (probe-file *art*) (save-observations)))
    (uiop:quit code)))
(main)
