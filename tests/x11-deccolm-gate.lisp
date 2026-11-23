;;;; Native DECCOLM ordering: withhold the actual window resize on private Xvfb.
(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(require :sb-bsd-sockets)
(asdf:load-system "xclhb")

(defpackage #:common-term.x11-deccolm (:use #:cl))
(in-package #:common-term.x11-deccolm)

(defparameter *artifacts* (pathname (first (uiop:command-line-arguments))))
(defvar *display*)
(defvar *client*)
(defvar *terminal*)
(defvar *phase*)
(defvar *window*)
(defvar *requests*)
(defvar *notifies*)
(defvar *command-id*)
(defvar *cell*)

(defun check (ok control &rest args)
  (unless ok
    (let ((*print-pretty* nil))
      (error "~A: ~A" *phase* (apply #'format nil control args)))))

(defun note (control &rest args)
  (let ((*print-pretty* nil))
    (format t "~A: ~?~%" *phase* control args))
  (finish-output))

(defun bounded (seconds label function)
  (handler-case (sb-ext:with-timeout seconds (funcall function))
    (sb-ext:timeout () (error "~A: timed out waiting for ~A" *phase* label))))

(defun await (label function &optional (seconds 5))
  (bounded seconds label
           (lambda ()
             (loop for value = (funcall function) when value return value
                   do (sleep 0.005)))))

(defun stop-process (process)
  (when process
    (when (uiop:process-alive-p process)
      (uiop:terminate-process process)
      (handler-case (sb-ext:with-timeout 2 (uiop:wait-process process))
        (sb-ext:timeout () (uiop:terminate-process process :urgent t))))
    (uiop:wait-process process)))

(defun child-command (command &optional (seconds 3))
  (let* ((id (incf *command-id*))
         (tmp (merge-pathnames "command.tmp" *phase*))
         (dest (merge-pathnames (format nil "command.~D" id) *phase*))
         (reply (merge-pathnames (format nil "reply.~D" id) *phase*)))
    (with-open-file (s tmp :direction :output :if-exists :error)
      (write-line command s))
    (rename-file tmp dest)
    (unless (string= command "quit")
      (await (format nil "child reply ~D (~A)" id command)
             (lambda ()
               (check (uiop:process-alive-p *terminal*) "terminal exited; see terminal.stderr")
               (when (probe-file reply)
                 (with-open-file (s reply) (read-line s))))
             seconds))))

(defun tty-size ()
  (let* ((reply (child-command "size"))
         (parts (uiop:split-string reply :separator '(#\Space))))
    (check (= 2 (length parts)) "invalid stty size reply ~S" reply)
    (mapcar #'parse-integer parts)))

(defun actual-pixels ()
  (let ((reply (xclhb:get-geometry-sync *client* *window*)))
    (list (xclhb:get-geometry-reply-width reply)
          (xclhb:get-geometry-reply-height reply))))

(defun pixels (grid)
  (list (* (first *cell*) (second grid)) (* (second *cell*) (first grid))))

(defun set-event-mask (window mask)
  (apply #'xclhb:change-window-attributes *client* window (ash 1 11)
         (append (make-list 11 :initial-element 0) (list mask 0 0 0)))
  (check (= mask (xclhb:get-window-attributes-reply-your-event-mask
                  (xclhb:get-window-attributes-sync *client* window)))
         "could not subscribe to events on #x~X" window))

(defconstant +width-height-mask+ (logior (ash 1 2) (ash 1 3)))

(defun configure (size)
  (xclhb:configure-window *client* *window* +width-height-mask+
                          0 0 (first size) (second size) 0 0 0)
  (xclhb:flush *client*))

(defun watch-configures ()
  (xclhb:set-event-handler
   *client* xclhb:+configure-request-event+
   (lambda (event)
     (when (= *window* (xclhb:configure-request-event-window event))
       (push event *requests*))))
  (xclhb:set-event-handler
   *client* xclhb:+configure-notify-event+
   (lambda (event)
     ;; X11 puts the SendEvent flag in the high bit of the raw event type.
     (when (and (= *window* (xclhb:configure-notify-event-window event))
                (not (logbitp 7 (aref (xclhb:client-input-buffer *client*) 0))))
       (push (list (xclhb:configure-notify-event-width event)
                   (xclhb:configure-notify-event-height event))
             *notifies*)))))

(defun arm ()
  ;; A round trip drains the observer's outstanding events. This is not a
  ;; clock or a dispatch barrier for the other X connection.
  (actual-pixels)
  (setf *requests* nil *notifies* nil))

(defun send-mode (command)
  (check (string= "sent" (child-command command)) "unexpected child mode acknowledgement"))

(defun hold-request (command before target &key probe)
  (let ((old (pixels before)) (wanted (pixels target)))
    (arm)
    (send-mode (or probe command))
    (let ((request (await "redirected ConfigureRequest"
                          (lambda () (xclhb:process-input *client*) (first *requests*)))))
      (check (= +width-height-mask+
                (logand +width-height-mask+ (xclhb:configure-request-event-value-mask request)))
             "ConfigureRequest did not specify width and height: ~S" request)
      (check (equal wanted (list (xclhb:configure-request-event-width request)
                                 (xclhb:configure-request-event-height request)))
             "unexpected ConfigureRequest for ~A; wanted ~S, got ~S" command wanted request))
    (note "intercepted ~A ConfigureRequest for #x~X, requested ~S pixels" command *window* wanted)
    ;; Each sample is taken after the server intercepted the resize and before
    ;; we let it perform the resize. Keep it held over several host iterations
    ;; to also catch a resize published asynchronously after the X request.
    (let ((end (+ (get-internal-real-time) (ceiling internal-time-units-per-second 10))))
      (loop
        (let* ((observed (tty-size)) (actual (actual-pixels)))
          (check (equal old actual) "X resized despite the held request: expected ~S, got ~S" old actual)
          (check (null *notifies*) "saw ConfigureNotify while the request was held: ~S" *notifies*)
          (check (equal before observed)
                 "HELD-ORDER ~A: X still ~S pixels, expected tty ~S, got ~S" command actual before observed)
          ;; The child polls the raw PTY without consuming any bytes. Even a
          ;; partial reply is a failure while we still own the trapped request.
          (when probe
            (let ((state (child-command "cpr-ready")))
              (check (not (string= "readable" state))
                     "HELD-CPR ~A: reply became readable before ConfigureRequest release (X ~S, tty ~S)"
                     command actual observed)
              (check (string= "pending" state) "unexpected CPR readiness result ~S" state)))
          (when (>= (get-internal-real-time) end)
            (note "held ~A; actual pixels ~S, tty ~S" command actual observed)
            (when probe (note "held ~A: no CPR readable before release" command))
            (return)))
        (sleep 0.005)))))

(defun converge (label before target &key (check-no-wm-order nil))
  (let ((wanted (pixels target)))
    (await label
           (lambda ()
             (let* ((observed (tty-size))
                    (actual (actual-pixels)))
               (check (member observed (list before target) :test #'equal)
                      "~A: tty reached unexpected ~S, expected ~S or ~S" label observed before target)
               (when (and check-no-wm-order (equal observed target))
                 (check (equal actual wanted)
                        "~A: new tty ~S followed by old/wrong X geometry ~S" label observed actual))
               (and (equal observed target) (equal actual wanted)
                    (member wanted *notifies* :test #'equal))))))
  (note "PASS ~A: genuine ConfigureNotify, X ~S pixels and tty ~S" label (pixels target) target))

(defun read-cpr-sample ()
  (let ((sample (child-command "sample-cpr")))
    (unless (string= sample "pending") sample)))

(defun check-cpr (label sample cursor grid)
  (destructuring-bind (row col) grid
    (check (string= sample (format nil "~A ~D ~D" cursor row col))
           "~A: expected CPR ~A with fresh tty ~S, got ~S" label cursor grid sample)))

(defun release-and-check-cpr (command before target)
  (configure (pixels target))
  ;; Read the queued CPR and its child-side ioctl result before waiting for
  ;; geometry convergence, otherwise the convergence wait can mask a race.
  (let ((sample (await "CPR after ConfigureRequest release" #'read-cpr-sample)))
    (check-cpr command sample "1;1" target)
    (converge (format nil "held DECCOLM ~A, then release" command) before target)
    (note "PASS CPR ~A: reply ~A, fresh tty ~S, X ~S pixels" command "1;1" target (actual-pixels))))
(defun redirected-checks ()
  (let ((normal '(24 80)) (wide '(24 132)) (adjusted '(26 96)))
    (hold-request "set" normal wide :probe "set-cpr")
    ;; X11 applies requests from the redirect owner directly.
    (release-and-check-cpr "set" normal wide)
    (hold-request "reset" wide normal :probe "reset-cpr")
    (release-and-check-cpr "reset" wide normal)
    (hold-request "set" normal wide)
    (configure (pixels adjusted))
    (converge "redirect owner adjusts rows and columns" normal adjusted)
    (arm)
    (configure (pixels normal))
    (converge "external resize after adjustment" adjusted normal)))

;; Use a fresh terminal so refusal starts with a fresh DECCOLM transition.
(defun refusal-checks ()
  (let ((normal '(24 80)) (wide '(24 132)))
    (hold-request "set" normal wide :probe "refusal-cpr")
    ;; Drop the configure without a synthetic event. CUP to 1;100 was in
    ;; the original write, so a deferred reply from a prematurely wide grid
    ;; would return column 100 rather than clamping to 80 after rollback.
    (let ((end (+ (get-internal-real-time) (* 2 internal-time-units-per-second)))
          (sample nil))
      (loop
        (unless sample
          (setf sample (read-cpr-sample))
          (when sample (check-cpr "timeout rollback" sample "1;80" normal)))
        (let* ((observed (tty-size)) (actual (actual-pixels)))
          (check (and (equal normal observed) (equal (pixels normal) actual) (null *notifies*))
                 "drop without notify: expected old geometry; X ~S, tty ~S, events ~S"
                 actual observed *notifies*))
        (when (>= (get-internal-real-time) end) (return))
        (sleep 0.025))
      (unless sample
        (setf sample (await "deferred CPR after refused resize" #'read-cpr-sample 3))
        (check-cpr "timeout rollback" sample "1;80" normal))
      (let* ((observed (tty-size)) (actual (actual-pixels)))
        (check (and (equal normal observed) (equal (pixels normal) actual) (null *notifies*))
               "geometry changed during the timeout cursor probe")
        (note "PASS CPR timeout: no notify, X ~S pixels, fresh tty ~S, CUP to 1;100 reported 1;80"
              actual observed)))
    (arm)
    (configure (pixels wide))
    (converge "late real configure after drop" normal wide)))

(defun no-wm-checks ()
  (let ((normal '(24 80)) (wide '(24 132)) (external '(27 93)))
    (arm)
    (send-mode "set")
    (converge "no WM DECCOLM set" normal wide :check-no-wm-order t)
    (arm)
    (send-mode "reset")
    (converge "no WM DECCOLM reset" wide normal :check-no-wm-order t)
    (arm)
    (bounded 5 "external xdotool windowsize"
             (lambda ()
               (uiop:run-program (list "env" (format nil "DISPLAY=~A" *display*)
                                       "xdotool" "windowsize" (write-to-string *window*)
                                       (write-to-string (first (pixels external)))
                                       (write-to-string (second (pixels external)))))))
    (converge "no WM external xdotool resize" normal external)))

(defun run-terminal (label redirect checks)
  (let* ((*phase* (merge-pathnames (format nil "~A/" label) *artifacts*))
         (*terminal* nil)
         (*window* nil)
         (*requests* nil)
         (*notifies* nil)
         (*command-id* 0)
         (*cell* nil)
         (*client* (xclhb:x-connect nil *display*)))
    (check *client* "failed connecting to private Xvfb")
    (ensure-directories-exist *phase*)
    (unwind-protect
         (let ((root (xclhb:screen-root (aref (xclhb:setup-roots
                                              (xclhb:client-server-information *client*)) 0))))
           (xclhb:set-default-error-handler *client* (lambda (error) (error "X protocol error: ~S" error)))
           ;; On this private server the terminal is the only client to map a
           ;; window. No-wm only observes and never acquires redirect.
           (if redirect
               (xclhb:set-event-handler *client* xclhb:+map-request-event+
                                        (lambda (e) (setf *window* (xclhb:map-request-event-window e))))
               (xclhb:set-event-handler *client* xclhb:+map-notify-event+
                                        (lambda (e) (setf *window* (xclhb:map-notify-event-window e)))))
           (set-event-mask root (ash 1 (if redirect 20 19)))
           (unless redirect
             (check (not (logbitp 20 (xclhb:get-window-attributes-reply-all-event-masks
                                     (xclhb:get-window-attributes-sync *client* root))))
                    "no-WM phase unexpectedly has a root redirect owner"))
           (setf *terminal*
                 (uiop:launch-program
                  (list "env" (format nil "DISPLAY=~A" *display*) "bin/common-term"
                        "--backend" "x11" "--renderer" "cpu" "--class" "T" "--rows" "24" "--columns" "80"
                        "--" "bash" "tests/fixtures/x11-deccolm-child.sh" (namestring *phase*))
                  :input "/dev/null" :output (merge-pathnames "terminal.stdout" *phase*)
                  :error-output (merge-pathnames "terminal.stderr" *phase*)))
           (note "launcher PID ~D" (uiop:process-info-pid *terminal*))
           (await "terminal window" (lambda ()
                                       (check (uiop:process-alive-p *terminal*) "terminal exited; see terminal.stderr")
                                       (xclhb:process-input *client*) *window*) 30)
           (watch-configures)
           (set-event-mask *window* (ash 1 17))
           (when redirect
             (xclhb:map-window *client* *window*)
             (xclhb:flush *client*))
           (await "child ready after terminal DSR"
                  (lambda ()
                    (check (uiop:process-alive-p *terminal*) "terminal exited; see terminal.stderr")
                    (probe-file (merge-pathnames "ready" *phase*))) 10)
           (check (equal '(24 80) (tty-size)) "initial PTY grid is not 24x80")
           (destructuring-bind (width height) (actual-pixels)
             (check (and (plusp width) (plusp height) (zerop (mod width 80)) (zerop (mod height 24)))
                    "initial X geometry ~Dx~D is not cell-aligned for 24x80" width height)
             (setf *cell* (list (/ width 80) (/ height 24))))
           (note "window #x~X ready: initial tty 24x80 and X ~S pixels" *window* (actual-pixels))
           (funcall checks)
           (child-command "quit")
           (check (zerop (bounded 5 "clean launcher exit" (lambda () (uiop:wait-process *terminal*))))
                  "launcher or child exited unsuccessfully; see terminal.stderr"))
      (unwind-protect (stop-process *terminal*)
        (xclhb:x-close *client*)))))

(defun run-gate ()
  (let ((*phase* :initializing-xvfb) (*display* nil) (server nil))
    (unwind-protect
         (progn
           (setf server (uiop:launch-program
                         '("Xvfb" "-displayfd" "1" "-screen" "0" "1920x1080x24" "-nolisten" "tcp" "-ac" "-noreset")
                         :input "/dev/null" :output :stream
                         :error-output (merge-pathnames "xvfb.stderr" *artifacts*)))
           (let ((number (bounded 8 "Xvfb displayfd" (lambda () (read-line (uiop:process-info-output server) nil)))))
             (check (and number (plusp (length number)) (every #'digit-char-p number))
                    "Xvfb did not publish a private display; see xvfb.stderr")
             (setf *display* (format nil ":~A" number)))
           (note "private display ~A, Xvfb PID ~D" *display* (uiop:process-info-pid server))
            (bounded 120 "complete native DECCOLM gate"
                    (lambda ()
                      (run-terminal "redirected" t #'redirected-checks)
                      (run-terminal "refused" t #'refusal-checks)
                      (run-terminal "no-wm" nil #'no-wm-checks)))
           (format t "PASS: X11 DECCOLM~%"))
      (stop-process server))))

;; Catch outside the cleanup scopes so batch SBCL reaps the launcher and
;; Xvfb before exiting on a failing assertion.
(handler-case (run-gate)
  (error (condition)
    (format *error-output* "FAIL: ~A~%" condition)
    (uiop:quit 1)))
