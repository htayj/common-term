;;;; scripts/sbcl --script tests/run-launcher-tests.lisp [pure] [transport] [compositor]
;;;; No section name runs all three. The transport section replaces the X11
;;;; transport of this disposable image; the compositor section starts a private
;;;; headless compositor and runs bin/lispterm against it as a subprocess.
(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system "lispterm/launcher")

(defpackage #:lispterm.launcher.tests (:use #:cl))
(in-package #:lispterm.launcher.tests)

(defparameter *root* (asdf:system-source-directory "lispterm"))
(defvar *checks* 0)

(defun check (name value &optional detail)
  (incf *checks*)
  (format t "~S~%" (list :check name :pass (not (null value)))) (finish-output)
  (unless value (error "Launcher check failed: ~S~@[ ~A~]" name detail)))

(defun eventually (function &optional (timeout 10))
  (let ((end (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop for value = (funcall function)
          when value return value
          when (> (get-internal-real-time) end) return nil
          do (sleep .02))))

;;; Owned child processes are identified by pid and kernel start time.

(defun process-fields (pid)
  "Fields of /proc/PID/stat from the state onward, or NIL without a process."
  (let ((stat (ignore-errors (uiop:read-file-string (format nil "/proc/~D/stat" pid)))))
    (when stat
      (remove "" (uiop:split-string (subseq stat (1+ (position #\) stat :from-end t))))
              :test #'equal))))
(defun process-parent (pid) (let ((fields (process-fields pid))) (and fields (parse-integer (second fields)))))
(defun process-start (pid) (nth 19 (process-fields pid)))
(defun process-live-p (pid start)
  "True while the process recorded as PID and START runs. A zombie has ended."
  (let ((fields (process-fields pid)))
    (and fields (equal start (nth 19 fields)) (string/= "Z" (first fields)))))
(defun kill-owned (pid start)
  (when (and pid (process-live-p pid start)) (sb-posix:kill pid sb-posix:sigkill)))

(defun read-pid (file)
  (let ((text (and (probe-file file) (ignore-errors (uiop:read-file-string file)))))
    (and text (parse-integer text :junk-allowed t))))

(defun child-argv (pidfile &key stubborn (then "exec sleep 300"))
  "A child that records its pid in PIDFILE. STUBBORN ignores the terminal hangup."
  (list "sh" "-c" (format nil "~:[~;trap '' HUP; ~]echo $$ > \"$0\"; ~A" stubborn then)
        (namestring pidfile)))

;;; Pure: which recorded errors mean that the window broke.

(defun pure-tests ()
  (let* ((ring (list (list :time 4 :operation :display-close :message "close failed")
                     (list :time 3 :operation :display :message "connection lost")
                     (list :time 2 :operation :pty :message "read failed")
                     (list :time 1 :operation :input-cancelled :message "cancelled")))
         (failures (lispterm::display-failures ring)))
    (check :display-failures-oldest-first
           (equal '(:display :display-close) (mapcar (lambda (entry) (getf entry :operation)) failures)))
    (check :ordinary-errors-are-not-display-failures
           (null (lispterm::display-failures (cddr ring))))
    (check :no-errors-no-failures (null (lispterm::display-failures nil)))
    (check :failure-line
           (equal "lispterm: display error: connection lost"
                  (lispterm::failure-line (first failures))))))

;;; Transport: the real host, PTY and child on a fake display transport.

(defvar *poll* (constantly nil) "Called by every fake poll: returns events or signals.")
(defvar *fail-close* nil)
(defmethod lispterm.display:backend-open ((backend (eql :x11)) display)
  (declare (ignore backend display)))
(defmethod lispterm.display:backend-close ((backend (eql :x11)) display)
  (declare (ignore backend display))
  (when *fail-close*
    (error 'lispterm.display:display-error :detail "Injected native close failure")))
(defmethod lispterm.display:backend-poll ((backend (eql :x11)) display timeout)
  (declare (ignore backend display timeout))
  (funcall *poll*))
(defmethod lispterm.display:backend-blit ((backend (eql :x11)) display framebuffer rectangles)
  (declare (ignore backend display framebuffer rectangles)))
(defmethod lispterm.display:backend-set-title ((backend (eql :x11)) display title)
  (declare (ignore backend display title)))

(defun run-fake-shell (directory name &key stubborn then poll fail-close)
  "Run the launcher's shell path; POLL acts once the child has recorded its pid.
Return the exit code, the launcher's stderr text, and whether the child is gone."
  (let* ((pidfile (merge-pathnames (format nil "~(~A~).pid" name) directory))
         (errors (make-string-output-stream))
         (pid nil) (start nil))
    (setf *fail-close* fail-close
          *poll* (lambda ()
                   (when (and poll (or pid (setf pid (read-pid pidfile))))
                     (unless start (setf start (process-start pid)))
                     (funcall poll))))
    (unwind-protect
         (let ((code (let ((*error-output* errors))
                       (lispterm::run-shell '(:backend :x11 :renderer :cpu :rows 5 :columns 20)
                                            (child-argv pidfile :stubborn stubborn
                                                                :then (or then "exec sleep 300"))))))
           (values code (get-output-stream-string errors)
                   (and (read-pid pidfile) (not (process-fields (read-pid pidfile))))))
      (setf *poll* (constantly nil) *fail-close* nil)
      ;; A display that could not close keeps its owner; release it now.
      (loop for host being the hash-values of lispterm::*hosts*
            do (ignore-errors (lispterm:close host)))
      (kill-owned pid start))))

(defun transport-tests ()
  (let ((directory (merge-pathnames (format nil ".build/launcher-tests-~D/" (sb-posix:getpid)) *root*)))
    (ensure-directories-exist directory)
    (unwind-protect
         (flet ((lost () (error 'lispterm.display:display-connection-lost
                                :detail "Injected transport loss"))
                (closed () (list (lispterm.display:make-close-event))))
           (multiple-value-bind (code errors gone-p) (run-fake-shell directory :user-close :poll #'closed)
             (check :user-close-exits-zero (eql 0 code))
             (check :user-close-is-silent (equal "" errors) errors)
             (check :user-close-reaps-child gone-p))
           (multiple-value-bind (code errors gone-p) (run-fake-shell directory :child-exit :then "exit 3")
             (check :child-exit-exits-zero (eql 0 code))
             (check :child-exit-is-silent (equal "" errors) errors)
             (check :child-exit-reaps-child gone-p))
           (multiple-value-bind (code errors gone-p) (run-fake-shell directory :display-error :poll #'lost)
             (check :display-error-exits-nonzero (eql 1 code))
             (check :display-error-is-printed
                    (search "lispterm: display error: Display: Injected transport loss" errors) errors)
             (check :display-error-reaps-child gone-p))
           ;; The display cannot close either, and the child ignores the hangup.
           (multiple-value-bind (code errors gone-p)
               (run-fake-shell directory :close-failure :poll #'lost :fail-close t :stubborn t)
             (check :close-failure-exits-nonzero (eql 1 code))
             (check :close-failure-prints-display-error
                    (search "lispterm: display error: Display: Injected transport loss" errors) errors)
             (check :close-failure-prints-display-close-error
                    (search "lispterm: display-close error: Display: Injected native close failure" errors)
                    errors)
             (check :close-failure-prints-close-error (search "lispterm: close error: " errors) errors)
             (check :close-failure-reaps-stubborn-child gone-p))
           (check :no-host-left (null (lispterm:status))))
      (uiop:delete-directory-tree directory :validate t))))

;;; Compositor: bin/lispterm as a subprocess of a private headless compositor.

(defun fixture (name &rest arguments)
  (apply #'uiop:symbol-call :lispterm.gpu.tests name arguments))
(defun swaymsg (&rest arguments)
  (fixture :fixture-command
           (list* (format nil "SWAYSOCK=~A" (uiop:getenv "SWAYSOCK")) "swaymsg" arguments)
           :ignore-error-status t))

(defun run-lispterm (directory name event &key stubborn)
  "Start bin/lispterm, wait for its window and child, call EVENT with the window
title, and wait for the exit. Return exit code, stderr text, and child liveness."
  (let* ((title (format nil "launcher-~(~A~)-~D" name (sb-posix:getpid)))
         (pidfile (merge-pathnames (format nil "~(~A~).pid" name) directory))
         (stdout (merge-pathnames (format nil "~(~A~).stdout" name) directory))
         (stderr (merge-pathnames (format nil "~(~A~).stderr" name) directory))
         (process (uiop:launch-program
                   (append (list (namestring (merge-pathnames "bin/lispterm" *root*))
                                 "--backend" "wayland" "--title" title "--rows" "10" "--columns" "40" "--")
                           (child-argv pidfile :stubborn stubborn))
                   :input nil :output stdout :error-output stderr))
         (launcher (uiop:process-info-pid process))
         (pid nil) (start nil))
    (unwind-protect
         (progn
           (check (list name :window-mapped)
                  (eventually (lambda () (and (uiop:process-alive-p process)
                                              (search title (swaymsg "-t" "get_tree" "-r"))))
                              90)
                  stderr)
           (check (list name :child-started) (setf pid (eventually (lambda () (read-pid pidfile)))))
           (setf start (process-start pid))
           (check (list name :child-owned-by-launcher) (eql launcher (process-parent pid)))
           (sleep 1)                    ; frames are presented before the event
           (check (list name :launcher-running) (uiop:process-alive-p process) stderr)
           (funcall event title)
           (check (list name :launcher-exited)
                  (eventually (lambda () (not (uiop:process-alive-p process))) 30))
           (let ((code (uiop:wait-process process))
                 (errors (uiop:read-file-string stderr)))
             (format t "LAUNCHER ~(~A~) exit=~D child-live=~S stdout=~A stderr=~A~%~A"
                     name code (process-live-p pid start) stdout stderr errors)
             (values code errors (process-live-p pid start))))
      (when (uiop:process-alive-p process) (uiop:terminate-process process :urgent t))
      (uiop:wait-process process)
      (kill-owned pid start))))

(defun compositor-tests ()
  (dolist (name '("DISPLAY" "WAYLAND_DISPLAY" "WAYLAND_SOCKET"))
    (check (list :no-inherited-endpoint name) (not (uiop:getenv name))))
  (dolist (file '("registry" "fixture-tests" "direct-compositor"))
    (load (merge-pathnames (format nil "tests/gpu/~A.lisp" file) *root*)))
  (fixture :call-with-direct-compositor
           (lambda (directory)
             (multiple-value-bind (code errors live-p)
                 (run-lispterm directory :window-close
                               (lambda (title) (swaymsg (format nil "[title=\"~A\"] kill" title))))
               (check :window-close-exits-zero (eql 0 code))
               (check :window-close-prints-no-error (not (search "lispterm:" errors)) errors)
               (check :window-close-child-gone (not live-p)))
             (multiple-value-bind (code errors live-p)
                 (run-lispterm directory :disconnect
                               (lambda (title) (declare (ignore title)) (swaymsg "exit"))
                               :stubborn t)
               (check :disconnect-exits-nonzero (and (integerp code) (plusp code)) code)
               (check :disconnect-prints-display-error (search "lispterm: display error: " errors) errors)
               (check :disconnect-child-gone (not live-p))))))

(handler-case
    (let ((sections (or (uiop:command-line-arguments) '("pure" "transport" "compositor"))))
      (dolist (section sections)
        (cond ((equal section "pure") (pure-tests))
              ((equal section "transport") (transport-tests))
              ((equal section "compositor") (compositor-tests))
              (t (error "Unknown launcher test section ~S" section))))
      (format t "launcher PASS ~D checks ~{~A~^ ~}~%" *checks* sections))
  (error (condition)
    (format *error-output* "Launcher verification failed: ~A~%" condition)
    (uiop:quit 1)))
