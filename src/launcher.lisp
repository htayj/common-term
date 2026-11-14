;;;; Executable entry points. Merely loading this file opens no terminal.
(in-package #:lispterm)

(defun launcher-options (arguments)
  (let ((options (list :backend :auto :renderer :auto :rows 30 :columns 100))
        (autolith-p nil) (argv nil))
    (labels ((value (name)
               (or (pop arguments) (error "Missing value after ~A" name)))
             (positive (name)
               (let ((number (parse-integer (value name))))
                 (unless (plusp number) (error "~A requires a positive integer" name))
                 number)))
      (loop while arguments for argument = (pop arguments) do
        (cond
          ((string= argument "--autolith") (setf autolith-p t))
          ((string= argument "--backend")
           (let ((name (value argument)))
             (setf (getf options :backend)
                   (cond ((string= name "auto") :auto)
                         ((string= name "x11") :x11)
                         ((string= name "wayland") :wayland)
                         (t (error "Unknown backend ~S" name))))))
          ((string= argument "--renderer")
           (let ((name (value argument)))
             (setf (getf options :renderer)
                   (cond ((string= name "auto") :auto)
                         ((string= name "gpu") :gpu)
                         ((string= name "cpu") :cpu)
                         (t (error "Unknown renderer ~S" name))))))
          ((string= argument "--display")
           (setf (getf options :display-name) (value argument)))
          ((string= argument "--title")
           (setf (getf options :title) (value argument)))
          ((string= argument "--rows")
           (setf (getf options :rows) (positive argument)))
          ((string= argument "--columns")
           (setf (getf options :columns) (positive argument)))
          ((string= argument "--") (setf argv arguments arguments nil))
          ((member argument '("-h" "--help") :test #'string=)
           (format t "Usage: lispterm [--autolith] [--backend auto|x11|wayland]~%                [--renderer auto|gpu|cpu]~%                [--display NAME] [--title TITLE] [--rows N] [--columns N]~%                [-- PROGRAM ARG...]~%")
           (return-from launcher-options (values nil nil :help)))
          (t (error "Unknown option ~S; use -- before program arguments" argument)))))
    (values options autolith-p argv)))

(defun rebuild-terminal-streams ()
  "Use fresh UTF-8 fd streams after the disposable process's dup2."
  (let ((input (sb-sys:make-fd-stream 0 :input t :buffering :none
                                   :external-format :utf-8 :auto-close nil))
        (output (sb-sys:make-fd-stream 1 :output t :buffering :line
                                    :external-format :utf-8 :auto-close nil))
        (errors (sb-sys:make-fd-stream 2 :output t :buffering :line
                                    :external-format :utf-8 :auto-close nil)))
    (setf *standard-input* input *standard-output* output *error-output* errors
          *trace-output* errors
          *terminal-io* (make-two-way-stream input output)
          *query-io* *terminal-io* *debug-io* *terminal-io*))
  nil)

(defun launch-autolith (options arguments)
  "Call actual Autolith MAIN synchronously in this heap's original thread."
  (unless (eq sb-thread:*current-thread* (sb-thread:main-thread))
    (error "Graphical Autolith must start on the original main thread"))
  (asdf:load-system "lispterm/autolith")
  (let* ((pty (lispterm.pty:open-pty :rows (getf options :rows)
                                     :columns (getf options :columns)))
         (host nil)
         (diagnostic-fd (sb-posix:dup 2))
         (diagnostics (sb-sys:make-fd-stream diagnostic-fd :output t
                                           :buffering :line :auto-close t)))
    (unwind-protect
         (progn
           (finish-output *standard-output*)
           (finish-output *error-output*)
           (lispterm.pty:attach-slave-to-stdio pty :disposable t)
           (rebuild-terminal-streams)
           (sb-posix:setenv "TERM" "xterm-256color" 1)
           (sb-posix:setenv "AUTOLITH_SESSION_STYLE" "direct" 1)
           ;; ASDF/Autolith bootstrap diagnostics retain a separate stream.
           (setf host (apply #'open :pty pty options))
           (uiop:symbol-call :lispterm.autolith :adopt-host host :hosted-main-p t)
           (let* ((package (find-package :lispterm.autolith))
                  (runtime-symbol (find-symbol "*AUTOMATIC-RUNTIME*" package))
                  (thread-symbol (find-symbol "*AUTOMATIC-MAIN-THREAD*" package))
                  (runtime (symbol-value (find-symbol "*RUNTIME*" package))))
             (progv (list runtime-symbol thread-symbol)
                 (list runtime sb-thread:*current-thread*)
               (format diagnostics "LISPTERM main pid=~D main-thread=~S tty0=~S tty1=~S tty2=~S session-style=~A~%"
                       (sb-posix:getpid)
                       (eq sb-thread:*current-thread* (sb-thread:main-thread))
                       (plusp (cffi:foreign-funcall "isatty" :int 0 :int))
                       (plusp (cffi:foreign-funcall "isatty" :int 1 :int))
                       (plusp (cffi:foreign-funcall "isatty" :int 2 :int))
                       (uiop:getenv "AUTOLITH_SESSION_STYLE"))
               (finish-output diagnostics)
               (uiop:symbol-call :autolith :main arguments))))
      ;; The slave remains the controlling terminal until process death. Closing
      ;; its master can SIGHUP this disposable process, so suppress HUP at exit.
      (sb-sys:enable-interrupt sb-unix:sighup :ignore)
      (when host
        (ignore-errors (uiop:symbol-call :lispterm.autolith :forget-host host))
        (ignore-errors (close host)))
      (ignore-errors (lispterm.pty:close-pty pty))
      (cl:close diagnostics))))

(defun main (&optional (arguments (uiop:command-line-arguments)))
  "Start a graphical shell, or Autolith in the current SBCL image."
  (multiple-value-bind (options autolith-p argv) (launcher-options arguments)
    (unless (eq argv :help)
      (if autolith-p
          (launch-autolith options argv)
          (let ((host (apply #'open :argv argv options)))
            (unwind-protect
                 (loop for status = (uiop:symbol-call :lispterm :status host)
                       until (or (member (getf status :state) '(:closed :detached))
                                 (member (getf status :process-state) '(:exited :signaled)))
                       do (sleep .05))
              (close host))))))
  nil)
