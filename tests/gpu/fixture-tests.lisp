(in-package #:lispterm.gpu.tests)

(defun fixture-wait (predicate description)
  (loop repeat 200 when (funcall predicate) do (return-from fixture-wait t)
        do (sleep 0.05))
  (error "Timed out waiting for ~A" description))

(defun fixture-read (path)
  (string-trim '(#\Newline #\Return #\Space) (uiop:read-file-string path)))

(defun fixture-command (arguments &rest options)
  (apply #'uiop:run-program
         (append '("env" "-u" "DISPLAY" "-u" "WAYLAND_DISPLAY" "-u" "WAYLAND_SOCKET"
                   "-u" "SWAYSOCK" "-u" "XAUTHORITY" "-u" "DBUS_SESSION_BUS_ADDRESS")
                 arguments)
         :directory *root* :input nil :output :string :error-output *error-output* options))

(defun fixture-socket-owner-p (pid path)
  "Associate an exact Unix endpoint with a socket FD in the owned server."
  (let ((inodes nil))
    (dolist (fd (uiop:directory-files (format nil "/proc/~D/fd/" pid)))
      (let ((target (ignore-errors (sb-posix:readlink fd))))
        (when (and target (uiop:string-prefix-p "socket:[" target))
          (push (subseq target 8 (1- (length target))) inodes))))
    (with-open-file (stream #p"/proc/net/unix")
      (loop for line = (read-line stream nil) while line
          for words = (remove "" (uiop:split-string line :separator '(#\Space)) :test #'equal)
            thereis (and (= 8 (length words)) (equal path (eighth words))
                         (member (seventh words) inodes :test #'equal))))))

(defun fixture-x11-check (directory)
  (let* ((number-file (merge-pathnames "display-number" directory))
         (log (merge-pathnames "xvfb.log" directory))
         (process (uiop:launch-program
                   (list "env" "-u" "DISPLAY" "-u" "WAYLAND_DISPLAY" "-u" "WAYLAND_SOCKET"
                         "-u" "XAUTHORITY" "Xvfb" "-displayfd" "1" "-screen" "0"
                         "1280x800x24" "-nolisten" "tcp")
                   :input nil :output number-file :error-output log))
         (pid (uiop:process-info-pid process)))
    (unwind-protect
         (progn
           (fixture-wait (lambda ()
                           (check (uiop:process-alive-p process) "Owned Xvfb exited during startup")
                           (and (probe-file number-file) (plusp (length (fixture-read number-file)))))
                         "private Xvfb")
           (let* ((number (parse-integer (fixture-read number-file)))
                  (display (format nil ":~D" number))
                  (socket (format nil "/tmp/.X11-unix/X~D" number)))
             (check (or (fixture-socket-owner-p pid socket)
                        (fixture-socket-owner-p pid (concatenate 'string "@" socket)))
                    "Private X11 socket is not owned by Xvfb ~D" pid)
             (let ((info (fixture-command (list "xdpyinfo" "-display" display))))
               (check (search "1280x800" info) "Private Xvfb dimensions were not reported"))
             (format t "FIXTURE X11 private ~A pid=~D exact socket ownership, xdpyinfo1280x800 verified~%"
                     display pid)))
      (when (uiop:process-alive-p process) (uiop:terminate-process process))
      (uiop:wait-process process)
      (check (not (uiop:process-alive-p process)) "Owned Xvfb survived cleanup"))))

(defun fixture-wayland-check (directory)
  (let* ((compositor (merge-pathnames "compositor/" directory))
         (runtime nil) (pid nil))
    (ensure-directories-exist compositor)
    (unwind-protect
         (progn
           (fixture-command (list "tests/wayland-gate.sh" "start" (namestring compositor)))
           (setf runtime (fixture-read (merge-pathnames "runtime-path" compositor))
                 pid (parse-integer (fixture-read (merge-pathnames "compositor.pid" compositor))))
           (let ((stat (sb-posix:stat runtime)))
             (check (and (= (sb-posix:stat-uid stat) (sb-posix:getuid))
                         (= #o700 (logand #o777 (sb-posix:stat-mode stat))))
                    "Compositor runtime is not private and owned"))
           (let ((socket (format nil "~A/wayland-1" runtime))
                 (ipc (format nil "~A/sway-ipc.~D.~D.sock" runtime (sb-posix:getuid) pid)))
             (check (and (fixture-socket-owner-p pid socket) (fixture-socket-owner-p pid ipc))
                    "Private compositor does not own both exact sockets"))
           (let ((environment (uiop:split-string
                               (uiop:read-file-string (format nil "/proc/~D/environ" pid))
                               :separator '(#\Null))))
             (check (and (member "WLR_BACKENDS=headless" environment :test #'equal)
                         (member "WLR_RENDERER=pixman" environment :test #'equal))
                    "Compositor is not the explicit headless fixture")
             (dolist (prefix '("DISPLAY=" "WAYLAND_DISPLAY=" "WAYLAND_SOCKET="))
               (check (notany (lambda (entry) (uiop:string-prefix-p prefix entry)) environment)
                      "Compositor inherited desktop endpoint ~A" prefix)))
           (check (search "HEADLESS-" (uiop:read-file-string (merge-pathnames "outputs.json" compositor)))
                  "Compositor has no verified headless output")
           (format t "FIXTURE Wayland pid=~D private mode700 runtime, both socket owners, headless output verified; pixman fixture only~%" pid))
      (fixture-command (list "tests/wayland-gate.sh" "stop" (namestring compositor)))
      (when pid
        (fixture-wait (lambda ()
                        (or (not (probe-file (format nil "/proc/~D/stat" pid)))
                            (let* ((stat (uiop:read-file-string (format nil "/proc/~D/stat" pid)))
                                   (end (position #\) stat :from-end t)))
                              (char= #\Z (char stat (+ end 2))))))
                      "owned compositor exit"))
      (when runtime
        (check (not (probe-file (format nil "~A/wayland-1" runtime))) "Wayland socket survived cleanup")
        ;; The only runtime removed is the one created and recorded by this fixture.
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname runtime) :validate t)))))

(defun gpu-fixtures-check ()
  (dolist (name '("DISPLAY" "WAYLAND_DISPLAY" "WAYLAND_SOCKET"))
    (check (not (uiop:getenv name)) "Run fixtures with ~A unset" name))
  (check (equal (truename *root*) (truename (uiop:getcwd)))
         "Fixture source root differs from selected implementation checkout")
  (check (equal (truename (asdf:system-source-directory "lispterm/gpu-tests")) (truename *root*))
         "GPU test system selected another checkout")
  (let ((directory (merge-pathnames
                    (format nil ".build/evidence/gpu-fixtures-~D-~D/" (sb-posix:getpid) (get-universal-time))
                    *root*)))
    (ensure-directories-exist directory)
    (fixture-x11-check directory)
    (fixture-wayland-check directory)
    (format t "FIXTURES source=~A artifacts=~A; owned Xvfb reaped, compositor exited and sockets removed~%"
            *root* directory))
  t)

(register-check "gpu-fixtures" #'gpu-fixtures-check)
