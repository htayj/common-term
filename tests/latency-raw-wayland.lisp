;;;; Small, raw-terminal stage measurement. No desktop endpoint or tmux.
(defpackage #:lispterm.latency.raw
  (:use #:cl #:lispterm.latency #:lispterm.latency.tokens)
  (:export #:run))
(in-package #:lispterm.latency.raw)

(defparameter *root* (asdf:system-source-directory "lispterm"))
(defparameter *directory* nil)
(defparameter *skip-audit-copy* nil)

(defun save-value (name value)
  (with-open-file (out (merge-pathnames name *directory*) :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-pretty* t)) (prin1 value out) (terpri out)))))

(defun source-hashes () (cl-user::raw-latency-source-hashes))

(defun wait-for (predicate description &optional (seconds 10))
  (let ((deadline (+ (lispterm.trace:monotonic-ns) (* seconds 1000000000))))
    (loop when (funcall predicate) return t
          when (> (lispterm.trace:monotonic-ns) deadline)
            do (error "Timed out waiting for ~A; evidence in ~A" description *directory*)
          do (sleep 0.01))))

(defun sway (&rest arguments)
  (lispterm.gpu.tests::fixture-command
   (list* (format nil "SWAYSOCK=~A" (uiop:getenv "SWAYSOCK")) "swaymsg" arguments)))

(defun inject-key (index)
  "Bracket one real virtual-keyboard injection into the owned compositor."
  (let ((before (lispterm.trace:monotonic-ns)))
    (uiop:run-program
     (list "env" "-u" "DISPLAY" "-u" "WAYLAND_SOCKET" "-u" "SWAYSOCK"
           "-u" "XAUTHORITY" "-u" "DBUS_SESSION_BUS_ADDRESS"
           (format nil "XDG_RUNTIME_DIR=~A" (uiop:getenv "XDG_RUNTIME_DIR"))
           (format nil "WAYLAND_DISPLAY=~A" (uiop:getenv "WAYLAND_DISPLAY"))
           "timeout" "5" "wtype" "-k" (string (code-char (key-byte index))))
     :directory *root* :input nil :output nil :error-output *error-output*)
    (list index before (lispterm.trace:monotonic-ns))))

(defun ready-p (host nonce)
  (let ((session (lispterm::host-session host)))
    (sb-thread:with-mutex ((lispterm::session-lock session))
      (screen-ready-p nonce (lispterm::session-screen session)))))

(defun grid-size (host)
  (let ((session (lispterm::host-session host)))
    (sb-thread:with-mutex ((lispterm::session-lock session))
      (let ((screen (lispterm::session-screen session)))
        (list :rows (lispterm.engine:rows screen) :columns (lispterm.engine:columns screen))))))

(defun read-probes (path)
  (with-open-file (in path)
    (let ((*read-eval* nil))
      (values (progn (read in) (loop for record = (read in nil) while record collect record))))))

(defun interval-summary (samples warm-up accessor)
  (let ((values (loop for sample across samples
                      when (and (>= (sample-index sample) warm-up)
                                (eq :matched (sample-status sample)))
                        collect (funcall accessor sample))))
    (when values (summarize values))))

(defun audit-hashes (samples)
  "Keep all three readbacks, including absent values, in the native receipt."
  (loop for sample across samples
        collect (list :index (sample-index sample)
                      :before (lispterm.latency::sample-before-hash sample)
                      :committed (lispterm.latency::sample-buffer-hash sample)
                      :settled (lispterm.latency::sample-settled-hash sample))))

(defun measure-case (name nonce interval &key (count 70) (warm-up 20) audit skip-copy)
  (let* ((slots 24)
         (recorder (make-recorder nonce slots :audit audit))
         (probe-log (merge-pathnames (format nil "~A-probe.sexp" name) *directory*))
         (host nil) (injections nil) (grid nil) (errors nil)
         (before (source-hashes)) (events nil) (receipt nil))
    (save-value (format nil "~A-source-preload.sexp" name) cl-user::*raw-latency-load-hashes*)
    (save-value (format nil "~A-source-before.sexp" name) before)
    (unless (string= before cl-user::*raw-latency-load-hashes*)
      (error "Source changed between load and measurement; resample"))
    (multiple-value-setq (events receipt)
          (call-with-recorder
           recorder
           (lambda ()
             (unwind-protect
                  (progn
                    (setf host
                          (lispterm:open
                           :backend :wayland :renderer :gpu :rows 30 :columns 80
                           :title (format nil "RAW-LATENCY-~A" nonce)
                           :argv (list (namestring (merge-pathnames ".build/deps/bin/sbcl" *root*))
                                       "--noinform" "--disable-debugger" "--script"
                                       (namestring (merge-pathnames "tests/latency-probe-child.lisp" *root*))
                                       nonce (write-to-string slots) (write-to-string count)
                                       (namestring probe-log))))
                    (wait-for (lambda () (ready-p host nonce)) "raw probe ready token")
                    (sleep 0.2)
                    (setf grid (grid-size host))
                    (unless (> (getf grid :rows) slots)
                      (error "Grid too small for token slots: ~S" grid))
                    ;; Only our one window exists in this private compositor.
                    (save-value (format nil "~A-focus.sexp" name)
                                (sway (format nil "[title=\"RAW-LATENCY-~A\"] focus" nonce)))
                    (save-value (format nil "~A-tree.sexp" name) (sway "-t" "get_tree" "-r"))
                     (when skip-copy (setf *skip-audit-copy* t))
                    (dotimes (index count)
                      (push (inject-key index) injections)
                      (sleep interval))
                    (wait-for (lambda () (probe-file probe-log)) "complete raw probe log")
                     (sleep 0.3)
                     (when audit
                       ;; Explicit settled redraw, outside timed measurements.
                       (let ((session (lispterm::host-session host)))
                         (sb-thread:with-mutex ((lispterm::session-lock session))
                           (setf (lispterm::session-force-frame session) t)))
                       (sleep 0.3))
                    (setf errors (getf (lispterm:status host) :error-count)))
               (when host
                 (save-value (format nil "~A-status.sexp" name) (lispterm:status host))
                 (lispterm:close host))
               (save-value (format nil "~A-injections.sexp" name) (reverse injections))))))
    (setf injections (nreverse injections))
    (save-value (format nil "~A-recorder.sexp" name) receipt)
    (save-value (format nil "~A-events.sexp" name) events)
    (save-value (format nil "~A-injections.sexp" name) injections)
    (let ((after (source-hashes)) (probes (read-probes probe-log)))
      (save-value (format nil "~A-source-after.sexp" name) after)
      (multiple-value-bind (samples matching)
          (match-run events :count count :slots slots :probes probes
                            :injections injections :input-path :display :nonce nonce)
        (save-value (format nil "~A-samples.sexp" name) samples)
        (let* ((copies (remove-if-not (lambda (event) (eq :copy (first event))) events))
               (report
                  (list :case name :claim (if audit :nontimed-native-audit :submission-stage-samples)
                        :audit-disagreements (when audit (getf (nth-value 1 (audit-agreement samples))
                                                               :disagreements))
                        :audit-hashes (when audit (audit-hashes samples))
                        :audit-completeness (when audit (nth-value 1 (audit-agreement samples)))
                        :recorder-admission (lispterm.latency.budgets:recorder-admission receipt events)
                       :frozen (string= before after) :error-count errors :grid grid
                       :requested-renderer :gpu :device "/dev/dri/renderD128"
                       :observed-paths (remove-duplicates (mapcar (lambda (event) (getf (cddr event) :path)) copies))
                       :observed-scales (remove-duplicates (mapcar (lambda (event) (getf (cddr event) :scale)) copies))
                       :copy-dimensions (remove-duplicates
                                         (mapcar (lambda (event) (list (getf (cddr event) :width)
                                                                      (getf (cddr event) :height))) copies)
                                         :test #'equal)
                        :count count :warm-up warm-up :measured (- count warm-up) :matching matching
                       :intervals-ns (run-summary samples :warm-up warm-up)
                       :dispatch-to-write-ns
                       (interval-summary samples warm-up
                                         (lambda (s) (- (sample-write s) (lispterm.latency::sample-dispatch s))))
                       :inject-to-write-lower-ns
                       (interval-summary samples warm-up
                                         (lambda (s) (first (getf (sample-intervals s) :input-to-write))))
                       :inject-to-write-upper-ns
                       (interval-summary samples warm-up
                                         (lambda (s) (second (getf (sample-intervals s) :input-to-write))))
                       :inject-to-commit-lower-ns
                       (interval-summary samples warm-up
                                         (lambda (s) (first (getf (sample-intervals s) :end-to-end))))
                       :inject-to-commit-upper-ns
                       (interval-summary samples warm-up
                                         (lambda (s) (second (getf (sample-intervals s) :end-to-end))))
                       :injection-bracket-ns (summarize (mapcar (lambda (r) (- (third r) (second r)))
                                                                              (nthcdr warm-up injections))))))
          (save-value (format nil "~A-report.sexp" name) report)
          (format t "~S~%" report) (finish-output)
          (unless (and (string= before after) (eql 0 errors)
                       (eq :complete (getf report :recorder-admission))
                       (eq :complete (getf matching :outcome)))
            (error "Incomplete or unfrozen stage measurement; inspect ~A" *directory*))
          (when audit
            (let ((hashes (audit-hashes samples))
                  (completeness (nth-value 1 (audit-agreement samples))))
              (unless (and (= count (length hashes))
                           (eql count (getf completeness :matched))
                           (eql count (getf completeness :complete))
                           (null (getf completeness :missing))
                           (every (lambda (row)
                                    (every (lambda (key) (integerp (getf row key)))
                                           '(:before :committed :settled))) hashes)
                           (if skip-copy (= count (length (getf completeness :disagreements)))
                               (null (getf completeness :disagreements))))
                (error "Native copy audit ~A has missing hashes or wrong differences: ~S"
                       name hashes))))
          (setf *skip-audit-copy* nil)
          (values report events probes injections))))))

(defun run (&key mode)
  "Record matched evidence or enforce fixed budgets. Neither mode claims photons."
  (unless (member mode '(:baseline-record :fixed-acceptance))
    (error "Choose explicit mode :BASELINE-RECORD or :FIXED-ACCEPTANCE"))
  (dolist (name '("DISPLAY" "WAYLAND_DISPLAY" "WAYLAND_SOCKET" "SWAYSOCK"))
    (when (uiop:getenv name) (error "Unset host endpoint ~A before this fixture" name)))
  (let* ((*directory* (merge-pathnames
                      (format nil ".build/raw-latency-~D-~D/" (sb-posix:getpid) (get-internal-real-time))
                      *root*))
         (lock (merge-pathnames ".build/raw-latency-native.lock" *root*))
         (lock-stream (open lock :direction :output :if-exists nil :if-does-not-exist :create)))
    (unless lock-stream (error "Raw fixture already locked: ~A" lock))
    (unwind-protect
         (progn
           (ensure-directories-exist *directory*)
           (save-value "mode.sexp" mode)
           (format t "RAW LATENCY ~S ARTIFACTS ~A~%" mode *directory*) (finish-output)
           (let* ((clock (clock-report)) (jitter (measure-wake-jitter))
                  (calibration (list :clock clock :clock-problems (clock-problems clock)
                                     :wake-jitter-ns (summarize jitter)
                                     :fixed-ceiling-ns +wake-jitter-ceiling-ns+
                                     :outcome (calibration-outcome jitter)))
                  (decision nil))
             (save-value "calibration.sexp" calibration)
             (when (or (clock-problems clock) (eq :unstable (calibration-outcome jitter)))
               (error "Clock or fixed wake-jitter calibration failed: ~S" calibration))
             (lispterm.gpu.tests::call-with-direct-compositor
              (lambda (fixture-directory)
                (save-value "fixture.sexp" (list :directory (namestring fixture-directory)
                                                :device "/dev/dri/renderD128" :scale 1))
                (sway "output" "HEADLESS-1" "mode" "800x600")
                (save-value "outputs.sexp" (sway "-t" "get_outputs" "-r"))
                ;; Preserve keyboard capabilities across short injector lifetimes.
                (let ((keeper (uiop:launch-program '("timeout" "90" "wtype" "-s" "60000")
                              :input nil :output (merge-pathnames "keyboard-keeper.out" *directory*)
                              :error-output (merge-pathnames "keyboard-keeper.err" *directory*)))
                      (real nil) (skipped nil))
                  (unwind-protect
                       (progn
                         (sleep 0.2)
                         (call-with-dma-copy-audit
                          (lambda (table)
                            (setf real (measure-case "audit-real" "AR" 0.12 :count 4 :warm-up 0
                                                     :audit (dma-audit 24 table))
                                  skipped (measure-case "audit-skipped" "AS" 0.12 :count 4 :warm-up 0
                                                        :audit (dma-audit 24 table) :skip-copy t))))
                         ;; Audit wrapper/readbacks are absent from timed evidence.
                         (multiple-value-bind (metadata events probes injections)
                             (measure-case "idle" "RI" 0.12)
                           (setf decision
                                 (lispterm.latency.budgets:evaluate-evidence
                                  events probes "RI" metadata calibration real skipped
                                  :injections injections
                                  :recorder-admission (getf metadata :recorder-admission)))))
                    (when (uiop:process-alive-p keeper) (uiop:terminate-process keeper))
                    (save-value "keyboard-keeper-exit.sexp" (uiop:wait-process keeper))))))
             (save-value "admission.sexp" decision)
             (format t "~S~%" (list :mode mode :submission-admission decision))
             (finish-output)
             (cond ((eq :rejected (getf decision :outcome)) :rejected)
                   ((eq mode :baseline-record)
                    (format t "BASELINE RECORDED. This is not low-latency acceptance.~%")
                    :baseline-recorded)
                   (t (getf decision :outcome)))))
      (setf *skip-audit-copy* nil)
      (close lock-stream)
      (delete-file lock))))
