;;;; One disposable native session. Test-tree instrumentation only.
(require :sb-posix)
(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system "lispterm")
(load (merge-pathnames "vttest-screen-frame.lisp" *load-truename*))
(load (merge-pathnames "vttest-vt52-sha256.lisp" *load-truename*))
;; Pure dependency: establish its package before reading qualified helper calls.
(load (merge-pathnames "vttest-vt52-navigation.lisp" *load-truename*))
(load (merge-pathnames "vttest-charsets-oracle.lisp" *load-truename*))
(load (merge-pathnames "vttest-charsets-native-contract.lisp" *load-truename*))
(load (merge-pathnames "vttest-charsets-pixels.lisp" *load-truename*))
;; Establish canonical VT52 packages before reading any qualified references.
(load (merge-pathnames "vttest-vt52-oracle.lisp" *load-truename*))
(load (merge-pathnames "vttest-vt52-capture-adapter.lisp" *load-truename*))
(defpackage #:vttest-charsets-recorder (:use #:cl))
(in-package #:vttest-charsets-recorder)
(defparameter *root* (uiop:ensure-directory-pathname (uiop:getenv "LISPTERM_VTTEST_RUN_ROOT")))
(defparameter *art* (merge-pathnames "recorder/" *root*))
(defparameter *backend* (ecase (intern (string-upcase (uiop:getenv "LISPTERM_VTTEST_BACKEND")) :keyword)
                          (:x11 :x11) (:wayland :wayland)))
(defparameter *scale*
  (let ((value (or (uiop:getenv "LISPTERM_VTTEST_SCALE") "1")))
    (cond ((string= value "1") 1)
          ((and (string= value "2") (eq *backend* :wayland)) 2)
          (t (error "Unsupported LISPTERM_VTTEST_SCALE ~s for backend ~(~a~)" value *backend*))))
  "Selected output scale: exactly \"1\" or \"2\", default 1; X11 accepts only 1.")
(defparameter *profile*
  (let ((value (or (uiop:getenv "LISPTERM_VTTEST_PROFILE") "charsets")))
    (cond ((string= value "charsets") :charsets)
          ((string= value "vt52") :vt52)
          (t (error "Unsupported LISPTERM_VTTEST_PROFILE ~s" value)))))
(defparameter *binary* (uiop:getenv "LISPTERM_VTTEST_EXECUTABLE"))
(defparameter *lock* (sb-thread:make-mutex :name "character-set recorder"))
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
(defun validate-vt52-native-failure (record root backend pause &optional envelope)
  "Validate the shared notify schema; VT52 accepts its actual pause range 0..3.
No pause ordering beyond equality with the recorder's current pause is inferred."
  (vttest-charsets-native-contract::tree record)
  (vttest-charsets-native-contract::properties
    record (append '(:kind :version :run :backend :pause :reason :stage :manifest)
                   (when envelope '(:origin))))
  (vttest-charsets-native-contract::need
    (and (eq :vttest-charsets-native-failure (getf record :kind))
         (eql 1 (getf record :version))
         (equal (namestring (truename root)) (getf record :run))
         (equal backend (getf record :backend))
         (member backend '("x11" "wayland") :test #'equal)
         (typep pause '(integer 0 3)) (eql pause (getf record :pause))
         (member (getf record :reason)
                 '("recorder" "request" "geometry" "authorization" "capture" "placement") :test #'equal)
         (member (getf record :stage) '(:placement :request :geometry :authorization :capture)))
    "Foreign/stale VT52 native failure")
  (vttest-charsets-native-contract::check-reference (getf record :manifest) root "MANIFEST.sexp")
  (when envelope
    (vttest-charsets-native-contract::check-reference
      (getf record :origin) root "native/native-failure-origin.sexp")
    (let ((origin (vttest-charsets-native-contract:read-data
                    (vttest-charsets-native-contract::local-path root "native/native-failure-origin.sexp"))))
      (validate-vt52-native-failure origin root backend pause)
      (vttest-charsets-native-contract::need
        (equal origin (loop for (k v) on record by #'cddr
                            unless (eq k :origin) append (list k v)))
        "Failure origin differs")))
  record)
(defun validate-profile-native-failure (record root backend pause &optional envelope)
  (ecase *profile*
    (:vt52 (validate-vt52-native-failure record root backend pause envelope))
    (:charsets (vttest-charsets-native-contract::validate-native-failure
                 record root backend pause envelope))))
(defun check-native-failure ()
  "Validate on the controlling thread, before accepting any simultaneous success."
  (let ((file (merge-pathnames "native/native-failure.sexp" *root*)))
    (when (probe-file file)
      (let ((reason
              (handler-case
                  (progn
                    (when *native-failure-seen* (error "Duplicate native failure"))
                    (setf *native-failure-seen* t)
                    (let ((record (vttest-charsets-native-contract:read-data file)))
                      (validate-profile-native-failure
                        record (namestring *root*) (string-downcase *backend*) *pause* t)
                      (save "native-failure-observed.sexp"
                        (list :origin (vttest-charsets-native-contract:reference
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
(defparameter *owned-pty* nil)

(defun bind-vt52-owned-pty (pty argv)
  "Bind the actual successfully spawned PTY, before the owner's first read."
  (let* ((process (lispterm.pty:pty-process pty))
         (pid (and process (lispterm.pty:process-pid process))))
    (unless (and (typep pty 'lispterm.pty:pty) (integerp pid) (plusp pid)
                 (equal argv (list *binary* "-l" (namestring (path "application.log")) "24x80.132")))
      (error 'native-abort :reason "VT52 spawn ownership is unproven"))
    (sb-thread:with-mutex (*lock*)
      (when *owned-pty*
        (error 'native-abort :reason "Duplicate VT52 owned spawn"))
      (setf *owned-pty* pty *owned-child* pid))))
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
                        (if (eq *profile* :vt52)
                            (bind-vt52-owned-pty (first args) (second args))
                            (let ((process (lispterm.pty:pty-process (first args))))
                              (setf *owned-child* (if process (lispterm.pty:process-pid process) :unknown))))))))))))))

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
        :expected-grid-size (list 24 80)
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
        :blink-phase (getf status :blink-phase) :blink-on-required nil
        :blink-phase-match (member (vttest-screen-frame:required-value status :blink-phase) '(t nil))))

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
          (let* ((status (observed-status host)) (actual (grid host))
                 (session (lispterm::host-session host))
                 (cursor (sb-thread:with-mutex ((lispterm::session-lock session))
                           (let ((screen (lispterm::session-screen session)))
                             (list :row (1+ (lispterm.engine:cursor-row screen))
                                   :column (1+ (lispterm.engine:cursor-column screen))))))
                 (details (predicate-details text actual
                             (getf (snapshot-metadata snapshot) :cursor) cursor
                            nil
                            status n stage))
                 (matches (if (and (eq *profile* :charsets) (eq stage :grid-wait))
                              (getf details :grid-match)
                              (and (every (lambda (key) (getf details key))
                                          '(:grid-match :cursor-match :settled :output-match :input-empty
                                            :blink-content-match :frame-complete :blink-phase-match))
                                   (or (eq *profile* :charsets)
                                       (eq t (vttest-screen-frame:required-value status :blink-phase)))))))
            (setf candidate (append (list :candidate-result (not (null matches))
                                         :candidate-ticks (get-internal-real-time)
                                         :candidate-event *sequence* :candidate-output *output*
                                         :candidate-input *input*) details))
            (when matches (if (eq stage :grid-wait) t status))))))))

(defun path (name) (merge-pathnames name *art*))
(defun write-receipt-value (value stream)
  "Write one receipt and LF; VT52 uses the admission reader's plain syntax."
  (if (eq *profile* :vt52)
      (with-standard-io-syntax
        (let ((*print-readably* nil) (*print-escape* t) (*print-base* 10)
              (*print-pretty* nil) (*print-length* nil) (*print-level* nil))
          (write value :stream stream) (terpri stream)))
      (let ((*print-pretty* nil) (*print-length* nil) (*print-level* nil))
        (write value :stream stream) (terpri stream))))

(defun save (name value)
  (with-open-file (s (path name) :direction :output :if-exists :error)
    (write-receipt-value value s)))

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
    (lambda (s) (write-receipt-value value s))))
(defun vt52-properties (value)
  "Bound and reject shared/cyclic trees before any plist traversal."
  (let ((seen (make-hash-table :test #'eq)) (todo (list value)) (nodes 0))
    (loop while todo for node = (pop todo) when (consp node) do
      (unless (and (not (gethash node seen)) (<= (incf nodes) 4096))
        (error 'native-abort :reason "VT52 record tree refusal"))
      (setf (gethash node seen) t)
      (push (car node) todo) (push (cdr node) todo)))
  (let ((keys nil))
    (loop for tail = value then (cddr tail) while tail do
      (unless (and (consp tail) (consp (cdr tail)) (keywordp (car tail))
                   (not (member (car tail) keys)))
        (error 'native-abort :reason "VT52 record properties refusal"))
      (push (car tail) keys)))
  value)
(defun observed-status (host)
  (let ((value (lispterm:status host)))
    (when (eq *profile* :vt52)
      (vt52-properties value)
      ;; A legitimate startup may have NIL geometry; no settled claim follows.
      (vt52-properties (getf value :geometry)))
    value))
(defun read-vt52-value (name)
  "Bound original UTF-8 bytes and forbid reader dispatch before parsing."
  (let ((file (path name)))
    (unless (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:lstat file)))
      (error 'native-abort :reason "VT52 nonregular native receipt"))
    (let* ((bytes
             (with-open-file (s file :element-type '(unsigned-byte 8))
               (let ((size (file-length s)))
                 (unless (<= 1 size 8192) (error "VT52 native receipt size refusal"))
                 (let ((v (make-array size :element-type '(unsigned-byte 8))))
                   (unless (and (= size (read-sequence v s)) (null (read-byte s nil nil)))
                     (error "VT52 native receipt readback refusal"))
                   v))))
           (text (sb-ext:octets-to-string bytes :external-format :utf-8))
           (quoted nil) (escape nil))
      (loop for c across text do
        (cond (escape (setf escape nil))
              ((and quoted (char= c #\\)) (setf escape t))
              ((and (not quoted) (find c '(#\\ #\| #\; #\#)))
               (error "VT52 native receipt syntax refusal"))
              ((char= c #\") (setf quoted (not quoted)))))
      (with-standard-io-syntax
        (let ((*read-eval* nil) (*readtable* (copy-readtable nil)))
          (multiple-value-bind (form end) (read-from-string text nil :eof)
            (unless (and (not (eq form :eof))
                         (every (lambda (c) (find c '(#\Space #\Tab #\Newline #\Return #\Page)))
                                (subseq text end)))
              (error "VT52 native receipt single-form refusal"))
            (vt52-properties form)))))))
(defun read-value (name)
  (if (eq *profile* :vt52)
      (read-vt52-value name)
      (with-open-file (s (path name))
        (let ((*read-eval* nil))
          (let ((value (read s))) (unless (eq (read s nil :eof) :eof) (error "Extra receipt form")) value)))))
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
(defun wire (kind bytes &optional source read-pty)
  (when (plusp (length bytes))
    (sb-thread:with-mutex (*lock*)
      (let* ((outp (eq kind :wire-out)) (start (if outp *output* *input*))
             (end (+ start (length bytes))))
        (with-open-file (s (path (if outp "output.raw" "input.raw")) :direction :output
                           :element-type '(unsigned-byte 8) :if-exists :append :if-does-not-exist :create)
          (write-sequence bytes s))
          (if (eq *profile* :vt52)
            ;; READ-PTY comes from this actual PTY-READ invocation, not global PID state.
            (let* ((process (and read-pty (lispterm.pty:pty-process read-pty)))
                   (pid (and process (lispterm.pty:process-pid process))))
              (event-unlocked kind :start start :end end :bytes (copy-seq bytes) :source source
                              :child-pid (and (eq read-pty *owned-pty*)
                                              (eql pid *owned-child*) pid)))
            (event-unlocked kind :start start :end end :bytes (copy-seq bytes) :source source))
        (if outp (setf *output* end) (setf *input* end))
        (values *sequence* start end)))))

(defun vt52-output-snapshot ()
  "Coherent complete raw octets and chronological events; never acquire session locks."
  (unless (eq *profile* :vt52)
    (error 'native-abort :reason "VT52 navigation requested in another profile"))
  (sb-thread:with-mutex (*lock*)
    (unless (and *owned-pty* (integerp *owned-child*) (plusp *owned-child*)
                 (<= 0 *output* 1048576))
      (error 'native-abort :reason "VT52 output ownership or size is unproven"))
    ;; WIRE closes its append stream before publishing its event/counter under
    ;; this same lock. No live output buffer needs flushing here.
    (let ((bytes (make-array *output* :element-type '(unsigned-byte 8))))
      (cond ((probe-file (path "output.raw"))
             (with-open-file (s (path "output.raw") :element-type '(unsigned-byte 8))
               (unless (and (= (file-length s) *output*)
                            (= (read-sequence bytes s) *output*)
                            (eq (read-byte s nil :eof) :eof))
                 (error 'native-abort :reason "VT52 raw output length changed"))))
            ((plusp *output*)
             (error 'native-abort :reason "VT52 raw output is missing")))
      (values bytes (reverse (copy-tree *events*))
              (list :start *output* :event *sequence* :input *input*
                    :child-pid *owned-child* :ticks (get-internal-real-time))))))

(defun vt52-navigation-checkpoint ()
  "Take immediately BEFORE one separately authorized Return; this sends no input."
  (multiple-value-bind (output events checkpoint) (vt52-output-snapshot)
    ;; Validate the coherent prefix even when no fresh marker is required yet.
    (let ((proof (vttest-vt52-navigation:vt52-navigation-check
                   output events (length output) (getf checkpoint :child-pid) :main-menu)))
      (when (eq (getf proof :state) :refuse)
        (error 'native-abort :reason "VT52 checkpoint provenance refused")))
    checkpoint))

(defun await-vt52-navigation (checkpoint stage &optional (timeout 30))
  "Read-only polling. Return gate proof and absolute ledger offsets, never input."
  (unless (and (member stage '(:outer-hold :main-menu))
               (realp timeout) (< 0 timeout 120))
    (error 'native-abort :reason "Invalid VT52 navigation wait"))
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop
      (check-native-failure)
      (multiple-value-bind (output events current) (vt52-output-snapshot)
        (unless (and (eql (getf checkpoint :child-pid) (getf current :child-pid))
                     (integerp (getf checkpoint :event))
                     (<= 0 (getf checkpoint :event) (getf current :event)))
          (error 'native-abort :reason "VT52 navigation checkpoint ownership changed"))
        (let ((proof (vttest-vt52-navigation:vt52-navigation-check
                       output events (getf checkpoint :start)
                       (getf checkpoint :child-pid) stage)))
          (ecase (getf proof :state)
            (:ready
             (return (append proof
                       (list :end (length output) :event (getf current :event)
                             :input (getf current :input) :child-pid (getf current :child-pid)
                             :ticks (getf current :ticks) :checkpoint (copy-list checkpoint)))))
            (:refuse
             (error 'native-abort :reason
                    (format nil "VT52 navigation refused: ~s" (getf proof :reasons))))
            (:pending nil))))
      (when (>= (get-internal-real-time) deadline)
        (error 'native-abort :reason "VT52 navigation timed out without authorization"))
      (sleep .01))))

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
                            (multiple-value-bind (seq start end) (wire :wire-out bytes nil (first args))
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
                          (multiple-value-bind (seq start end) (wire :wire-in (subseq bytes 0 n) *write-source* pty)
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
(defun snapshot-metadata (snapshot)
  (ecase *profile*
    (:charsets (vttest-charsets-oracle:snapshot-metadata snapshot))
    (:vt52 (vttest-vt52-oracle:snapshot-metadata snapshot))))
(defun require-snapshot (snapshot)
  (ecase *profile*
    (:charsets (vttest-charsets-oracle:require-complete snapshot))
    (:vt52
     (unless (and (eql 2 (vttest-vt52-oracle:snapshot-pause snapshot))
                  (eq :complete-reference (getf (snapshot-metadata snapshot) :status)))
       (error "Incomplete VT52 reference"))
     snapshot)))
(defun expected-text (snapshot)
  (with-output-to-string (s)
    (loop for cell across (ecase *profile*
                            (:charsets (vttest-charsets-oracle:snapshot-cells snapshot))
                            (:vt52 (vttest-vt52-oracle:snapshot-cells snapshot)))
          for i from 1 do
      (write-char (code-char (vttest-screen-oracle:cell-code cell)) s)
      (when (zerop (mod i (ecase *profile*
                            (:charsets (vttest-charsets-oracle:snapshot-columns snapshot))
                            (:vt52 (vttest-vt52-oracle:snapshot-columns snapshot)))))
        (terpri s)))))
(defun cursor-matches-p (host snapshot)
  (let* ((expected (getf (snapshot-metadata snapshot) :cursor))
         (session (lispterm::host-session host)))
    (sb-thread:with-mutex ((lispterm::session-lock session))
      (let ((screen (lispterm::session-screen session)))
        (and (= (getf expected :row) (1+ (lispterm.engine:cursor-row screen)))
             (= (getf expected :column) (1+ (lispterm.engine:cursor-column screen)))
             (or (eq *profile* :charsets)
                 (eq (getf expected :visible) (nerimux/terminal:screen-cursor-visible screen))))))))
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

(defun control-file-sha256 (n suffix)
  "Hash the exact closed original file, not a printed reconstruction."
  (with-open-file (s (path (pause-name n suffix)) :element-type '(unsigned-byte 8))
    (let ((size (file-length s)))
      (unless (<= 1 size 8192) (error "Control input size refusal: ~a" suffix))
      (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
        (unless (and (= size (read-sequence bytes s)) (null (read-byte s nil nil)))
          (error "Control input readback refusal: ~a" suffix))
        (vttest-vt52-sha256:sha256-hex bytes)))))

(defun vt52-control-cursor (host status target when linked seq)
  "Sequential engine bracket, not atomic status, presentation or scanout."
  (let* ((output-before (sb-thread:with-mutex (*lock*) *output*))
         (session (lispterm::host-session host))
         (sample
           (sb-thread:with-mutex ((lispterm::session-lock session))
             (let ((screen (lispterm::session-screen session)))
               (list (1+ (lispterm.engine:cursor-row screen))
                     (1+ (lispterm.engine:cursor-column screen))
                     (nerimux/terminal:screen-cursor-visible screen)
                     (lispterm::session-bytes-received session)))))
         ;; Never call STATUS or acquire recorder lock while holding session lock.
         (after (observed-status host))
         (output-after (sb-thread:with-mutex (*lock*) *output*)))
    (unless (and (= target output-before output-after (fourth sample))
                 (eql target (getf status :bytes-received))
                 (eql target (getf after :bytes-received))
                 (= 12 (first sample)) (= 14 (second sample)) (eq t (third sample)))
      (error "VT52 cursor/output bracket refusal: ~s" sample))
    (list :seq seq :kind :cursor-sample :when when :row (first sample)
          :col (second sample) :visible (third sample) :linked-sha256 linked)))

(defun capture-hold (host n snapshot)
  (unless (eql n (ecase *profile* (:charsets 1) (:vt52 2)))
    (error "Capture pause does not match profile ~s" *profile*))
  (require-snapshot snapshot)
  (let* ((text (expected-text snapshot))
         (content nil)
         (before (await-target host snapshot n :capture-hold))
         (target (getf before :bytes-received)) (ready nil) (pre nil) (final nil) (native nil)
         (control-p (and (eq *profile* :vt52) (= n 2)))
         (control nil) (pre-hash nil) (final-hash nil))
    (save (pause-name n "grid.sexp") (grid host))
    (save (pause-name n "before.sexp") before)
    (event :source-endpoint :status :unverified :reason :full-session-wire-not-yet-admitted)
    (lispterm:invalidate host)
    (setf ready (await host (lambda ()
                             (let ((st (observed-status host)))
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
          (format s "~d ~d ~d ~d ~d ~d ~d~%" n window (getf g :actual-width) (getf g :actual-height) *scale* target *ack*)))
      (await host (lambda () (probe-file (path (pause-name n "native-ready.sexp")))))
      (let ((receipt (read-value (pause-name n "native-ready.sexp"))))
        (unless (and (eql 3 (getf receipt :version)) (eq *backend* (getf receipt :backend))
                     (eql n (getf receipt :pause)) (eq t (getf receipt :owned))
                     (eql 0 (getf receipt :capture-count)) (eql *scale* (getf receipt :scale))
                     (eql (getf g :actual-width) (getf receipt :width))
                     (eql (getf g :actual-height) (getf receipt :height))
                     (eql window (getf receipt (if (eq *backend* :x11) :window-id :surface-id))))
          (error "Native READY identity/geometry refusal")))
      (setf pre (observed-status host))
      (save (pause-name n "pre.sexp") pre)
      (when control-p
        (setf pre-hash (control-file-sha256 n "pre.sexp"))
        (push (list :seq 1 :kind :pre-written :sha256 pre-hash) control)
        (push (vt52-control-cursor host pre target :before-authorization pre-hash 2) control))
      (unless (and (equal ready pre) (= target *output*) (string= text (grid host))
                   (cursor-matches-p host snapshot)
                   (or (eq *backend* :x11) (completed-wayland-frame-p pre target)))
        (error "READY/PRE or target changed before authorization"))
      (when control-p
        (push (list :seq 3 :kind :authorize
                    :request-sha256 (control-file-sha256 n "request")
                    :request-sexp-sha256 (control-file-sha256 n "request.sexp")
                    :native-ready-sha256 (control-file-sha256 n "native-ready.sexp")) control)
        (unless (= target (sb-thread:with-mutex (*lock*) *output*))
          (error "VT52 output advanced before GO")))
      (touch (pause-name n "go"))
      (await host (lambda () (probe-file (path (pause-name n "done")))))
      (when control-p
        (push (list :seq 4 :kind :done :native-sha256 (control-file-sha256 n "native.sexp")) control))
      (setf final (observed-status host))
      (save (pause-name n "final.sexp") final)
      (when control-p
        (setf final-hash (control-file-sha256 n "final.sexp"))
        (push (list :seq 5 :kind :final-written :sha256 final-hash) control)
        (push (vt52-control-cursor host final target :after-done final-hash 6) control))
      (setf native (read-value (pause-name n "native.sexp"))))
    (vttest-screen-frame:validate-frame before ready pre final native
      :pause n :target target :blink-content content :endpoint nil :ack-output *ack* :surface-id *surface*
      :scale *scale*)
    (unless (and (= target *output*) (string= text (grid host)) (cursor-matches-p host snapshot)
                 (or (eq *backend* :x11) (completed-wayland-frame-p final target)))
      (error "Final source target changed"))
    ;; Publish only a complete, validated capture trace, before mandatory admission.
    ;; The adapter reads these ORIGINAL closed files, never reconstructed records.
    (when control-p
      (publish-request-value (pause-name n "control.sexp") (nreverse control)))
    (let ((comparison
            (ecase *profile*
              (:charsets
               (vttest-charsets-pixels:compare-capture
                 (namestring *root*) n snapshot before ready pre final native target *ack* *surface* :scale *scale*))
              (:vt52
               (let ((report (vttest-vt52-capture-adapter:check-vt52-capture-directory
                              *art* :pause n :scale *scale*)))
                 (save (pause-name n "comparison.sexp") report)
                 (unless (eq :pass (getf report :verdict))
                   (error "Mandatory VT52 capture comparison refused: ~s" (getf report :reasons)))
                 report)))))
      (list :pause n :pixels :pass :bytes :unverified :timing :pass
            :frame :pass :target :recognized-navigation-only :native native :comparison comparison
            :evidence (loop for suffix in '("before.sexp" "ready.sexp" "pre.sexp" "final.sexp" "native.sexp" "presentation.sexp")
                            collect (pause-name n suffix))))))

(defun raw (name)
  (with-open-file (s (path name) :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence v s) v)))

(defun record-launch-identity (host)
  (let* ((status (observed-status host)) (pid (getf status :child-pid))
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

(defun await-vt52-first-hold (host checkpoint)
  "vt52.c:149-157, fresh source strings at their exact one-based grid positions."
  (let ((markers '((10 16 "The screen should be cleared, and have a centered")
                   (11 16 "rectangle of \"*\"s with \"!\"s on the inside to the")
                   (12 16 "left and right. Only this, and")
                   (13 16 "Push <RETURN>"))))
    (await host
      (lambda ()
        (multiple-value-bind (output events current) (vt52-output-snapshot)
          (let ((proof (vttest-vt52-navigation:vt52-navigation-check
                          output events (getf checkpoint :start) (getf current :child-pid) :first-hold)))
            (when (eq :refuse (getf proof :state))
              (error 'native-abort :reason "VT52 first-hold provenance refused"))
          (let ((position (getf checkpoint :start)) (text (grid host))
                (status (observed-status host)))
              (and (eq :ready (getf proof :state))
                   (eql (getf checkpoint :child-pid) (getf current :child-pid))
                 (every (lambda (marker)
                          (destructuring-bind (row column string) marker
                            (let* ((raw (map 'vector #'char-code string))
                                   (found (search raw output :start2 position))
                                   (cell (+ (* (1- row) 81) (1- column))))
                              (when found (setf position (+ found (length raw))))
                              (and found (<= (+ cell (length string)) (length text))
                                   (string= string text :start2 cell :end2 (+ cell (length string)))))))
                        markers)
                 (vttest-screen-frame:settled-p status)
                 (eql (getf current :start) (getf status :bytes-received))
                 (eql 0 (getf status :input-pending))
                   (append proof (list :checkpoint (copy-list checkpoint)
                        :end (length output) :event (getf current :event)
                        :input (getf current :input) :child-pid (getf current :child-pid)
                        :ticks (getf current :ticks)))))))))))
(defun run-main ()
  (when (probe-file *art*) (error "Recorder directory already exists"))
  (ensure-directories-exist (path ".reserve"))
  (unless (and (uiop:absolute-pathname-p *binary*) (probe-file *binary*)) (error "Missing built executable"))
  (install-startup-observers)
  (install-wire-recorder) (install-presentation-recorder)
  (let ((host nil) (checks nil) (failure nil) (attempted 0) (reached 0) (captured 0)
        (released 0) (cleanup nil) (native-aborted nil) (accounting :unverified)
        (navigation (loop for p from 1 to 3 collect (list :pause p :reached nil :released nil)))
        (outer-proof nil) (menu-proof nil))
    (unwind-protect
         (handler-case
             (progn
               (setf host (lispterm:open :backend *backend* :rows 24 :columns 80
                                        :title (ecase *profile*
                                                 (:charsets "Lispterm-charsets-owned-run")
                                                 (:vt52 "Lispterm-vt52-owned-run"))
                                        :argv (list *binary* "-l" (namestring (path "application.log")) "24x80.132")))
               (record-launch-identity host)
               ;; Native observes initial placement before any target or BEFORE transaction.
               (publish-request-value "placement-request.sexp" (list :pid (sb-posix:getpid)))
               (await host (lambda () (probe-file (path "placement-ready.sexp"))))
               (await host (lambda () (search "Enter choice number" (grid host))))
               (ecase *profile*
                 (:charsets
                  (send host (format nil "3~c" #\Return))
                  (loop for n from 1 to 1 do
                    (setf *pause* n attempted n)
                    (let ((snapshot (vttest-charsets-oracle:require-complete (vttest-charsets-oracle:character-set-snapshot))))
                      (await-target host snapshot n :grid-wait)
                      (incf reached)
                      (push (capture-hold host n snapshot) checks)
                      (incf captured)
                      (send host (string #\Return)) (incf released))))
                 (:vt52
                  (let ((entry (vt52-navigation-checkpoint))
                        (snapshot (require-snapshot (vttest-vt52-oracle:vt52-snapshot))))
                    (send host (format nil "7~c" #\Return))
                    (setf *pause* 1)
                    (let ((proof (await-vt52-first-hold host entry)))
                      (setf (getf (first navigation) :reached) t)
                      (event :navigation-reached :hold 1 :proof proof))
                    (send host (string #\Return))
                    (setf (getf (first navigation) :released) t)
                    (event :navigation-released :hold 1)
                    (setf *pause* 2 attempted 1)
                    (await-target host snapshot 2 :grid-wait)
                    (incf reached)
                    (setf (getf (second navigation) :reached) t)
                    (event :navigation-reached :hold 2)
                    (push (capture-hold host 2 snapshot) checks)
                    (incf captured)
                    ;; The checkpoint is immediately before the single local Return2.
                    (let ((checkpoint (vt52-navigation-checkpoint)))
                      (send host (string #\Return))
                      (setf (getf (second navigation) :released) t)
                      (event :navigation-released :hold 2)
                      (setf outer-proof (await-vt52-navigation checkpoint :outer-hold)))
                    (setf *pause* 3 (getf (third navigation) :reached) t)
                    (event :navigation-reached :hold 3 :proof outer-proof)
                    ;; This NEW checkpoint cannot reuse output from the outer hold.
                    (let ((checkpoint (vt52-navigation-checkpoint)))
                      (send host (string #\Return))
                      (setf (getf (third navigation) :released) t)
                      (event :navigation-released :hold 3)
                      (setf menu-proof (await-vt52-navigation checkpoint :main-menu)))
                    (event :navigation-main-menu :proof menu-proof))))
               (setf *pause* 0)
               (await host (lambda () (search "Enter choice number" (grid host))))
               (send host (format nil "0~c" #\Return))
               (await host (lambda () (eq :exited (getf (observed-status host) :process-state))))
               (setf cleanup :graceful-exit))
           (error (c)
             (setf native-aborted (typep c 'native-abort))
             (setf failure (princ-to-string c))
             (save "abort.sexp" (list :pause *pause* :reason failure))
             (touch "ABORT")
             (when (and (plusp *pause*)
                        (or (eq *profile* :charsets)
                            (and (= *pause* 2) (not (find 2 checks :key (lambda (v) (getf v :pause)))))))
               (push (list :pause *pause* :pixels :unverified :bytes :unverified :timing :fail
                           :frame :fail :reason failure :evidence (pause-name *pause* "")) checks))))
      ;; After failure there are no more captures or target PASS claims.
      (when host
        (when native-aborted (setf cleanup :cooperative-native-abort))
        (when (and failure (not native-aborted) (eq *profile* :charsets))
          (handler-case
              (progn
                (when (plusp reached)
                  (loop for n from (1+ released) to 1 do
                    (send host (string #\Return)) (sleep .1)))
                (await host (lambda () (search "Enter choice number" (grid host))) 5)
                (send host (format nil "0~c" #\Return))
                (await host (lambda () (eq :exited (getf (observed-status host) :process-state))) 5)
                (setf cleanup :graceful-exit-after-failure))
            (error (c) (setf cleanup (list :graceful-exit-unverified (princ-to-string c))))))
        (save "terminal-status.sexp" (observed-status host))
        (handler-case (lispterm:close host)
          (error (c) (setf failure (or failure (princ-to-string c))
                           cleanup (list cleanup :close-error (princ-to-string c)))))))
    (handler-case
        (when (and (probe-file (path "output.raw")) (probe-file (path "input.raw")))
          (vttest-screen-frame:validate-events (reverse *events*) (raw "output.raw") (raw "input.raw"))
          (setf accounting :pass))
      (error (c) (setf accounting (list :fail (princ-to-string c)) failure (or failure "Wire accounting failure"))))
    (unless (or failure
                (and (= attempted 1) (= reached 1) (= captured 1)
                     (eq cleanup :graceful-exit)
                     (= (length checks) 1)
                     (every (lambda (c) (every (lambda (k) (eq :pass (getf c k)))
                                                '(:pixels :timing :frame))) checks)))
      (setf failure (ecase *profile*
                      (:charsets "Incomplete character-set run")
                      (:vt52 "Incomplete VT52 run"))))
    (when (and (eq *profile* :vt52) (not failure)
               (not (and outer-proof menu-proof
                         (every (lambda (n) (and (getf n :reached) (getf n :released))) navigation))))
      (setf failure "Incomplete VT52 navigation"))
    (let ((result
            (append
              (list :kind (ecase *profile* (:charsets :vttest-charsets-run) (:vt52 :vttest-vt52-run))
                    :version 2 :backend (string-downcase *backend*)
                    :status (if failure :fail :pass) :attempted attempted :reached reached
                    :captured (if (eq *profile* :charsets)
                                  (loop for n from 1 to attempted count (probe-file (path (pause-name n "actual.png"))))
                                  (if (probe-file (path (pause-name 2 "actual.png"))) 1 0))
                    :frame-verified captured :bytes :unverified :wire-accounting accounting
                    :reason (or failure (ecase *profile*
                                          (:charsets "One complete character-set comparison passed")
                                          (:vt52 "One complete VT52 comparison and three navigation holds passed")))
                    :cleanup cleanup :checks
                    (loop for n in (ecase *profile* (:charsets '(1)) (:vt52 '(2))) collect
                      (or (find n checks :key (lambda (v) (getf v :pause)))
                          (list :pause n :pixels :unverified :bytes :unverified :timing :unverified)))
                    :evidence '("events.sexp" "output.raw" "input.raw" "application.log" "launch.sexp" "terminal-status.sexp"))
              (when (eq *profile* :vt52)
                (list :profile :vt52 :scale *scale* :navigation-pauses '(1 2 3)
                      :navigation navigation
                      :navigation-proofs (list :outer-hold outer-proof :main-menu menu-proof)
                      :unverified-count (if failure 1 0))))))
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
