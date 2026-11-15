;;;; A private tmux server for latency runs, and a reference client on a plain PTY.
;;;; The server has its own socket directory and no configuration file; it never
;;;; sees the user's servers. Load after tests/latency-measure.lisp.
(require :sb-posix)
(in-package #:lispterm.latency)
(export '(tmux-program tmux-version start-private-tmux stop-private-tmux private-tmux-running-p
          private-tmux-directory private-tmux-socket private-tmux-attach-argv
          private-tmux-environment private-tmux-client-environment
          start-reference-client stop-reference-client))

(defun tmux-program ()
  "The user's installed tmux, which the project does not pin."
  (let ((path (merge-pathnames ".nix-profile/bin/tmux" (user-homedir-pathname))))
    (unless (probe-file path) (error 'measurement-error :detail "no tmux in ~/.nix-profile/bin"))
    (sb-ext:native-namestring (truename path))))

(defstruct private-tmux program directory name (session "probe"))

(defun private-tmux-environment (server)
  "The process environment without TMUX, with the server's own socket directory."
  (cons (format nil "TMUX_TMPDIR=~A" (private-tmux-directory server))
        (remove-if (lambda (entry)
                     (or (eql 0 (search "TMUX=" entry)) (eql 0 (search "TMUX_TMPDIR=" entry))
                         (eql 0 (search "TMUX_PANE=" entry))))
                   (sb-ext:posix-environ))))

(defun private-tmux-client-environment (server)
  "Environment of a client attached through a plain PTY."
  (cons "TERM=xterm-256color"
        (remove-if (lambda (entry) (eql 0 (search "TERM=" entry)))
                   (private-tmux-environment server))))

(defun private-tmux-socket (server)
  (format nil "~A/tmux-~D/~A" (private-tmux-directory server) (sb-posix:getuid)
          (private-tmux-name server)))

(defun run-tmux (server &rest arguments)
  "Run one tmux command against SERVER; return its exit code and output."
  (let* ((output (make-string-output-stream))
         (process (sb-ext:run-program
                   (private-tmux-program server)
                   (list* "-L" (private-tmux-name server) "-f" "/dev/null" arguments)
                   :environment (private-tmux-environment server)
                   :input nil :output output :error output)))
    (values (sb-ext:process-exit-code process) (get-output-stream-string output))))

(defun tmux-version ()
  (let ((output (make-string-output-stream)))
    (sb-ext:run-program (tmux-program) '("-V") :input nil :output output :error nil)
    (string-trim '(#\Space #\Newline) (get-output-stream-string output))))

(defun private-tmux-running-p (server)
  (eql 0 (run-tmux server "has-session" "-t" (private-tmux-session server))))

(defun stop-private-tmux (server)
  "Kill the server and remove its directory. Signals when either survives."
  (run-tmux server "kill-server")
  (loop repeat 100 while (probe-file (private-tmux-socket server)) do (sleep 0.02))
  (let ((directory (pathname (format nil "~A/" (private-tmux-directory server)))))
    (when (probe-file directory) (sb-ext:delete-directory directory :recursive t))
    (when (or (probe-file directory) (private-tmux-running-p server))
      (error 'measurement-error :detail "private tmux server or its directory survived cleanup"))))

(defun start-private-tmux (command &key (columns 80) (rows 24) (status nil))
  "Start a detached session running COMMAND, a list of program and arguments."
  (let* ((program (tmux-program))
         (directory (sb-posix:mkdtemp
                     (format nil "~A/lispterm-latency-XXXXXX"
                             (string-right-trim "/" (or (sb-posix:getenv "TMPDIR") "/tmp")))))
         (server (make-private-tmux :program program :directory directory
                                    :name (format nil "latency-~D" (sb-posix:getpid))))
         (started nil))
    (unwind-protect
         (progn
           ;; The size is fixed by hand so that two clients cannot resize the window.
           (multiple-value-bind (code output)
               (apply #'run-tmux server "new-session" "-d" "-s" (private-tmux-session server)
                      "-x" (princ-to-string columns) "-y" (princ-to-string rows) command)
             (unless (eql code 0)
               (error 'measurement-error :detail (format nil "tmux new-session: ~A" output))))
           (dolist (option `(("status" ,(if status "on" "off")) ("escape-time" "0")
                             ("window-size" "manual")))
             (unless (eql 0 (run-tmux server "set-option" "-g" (first option) (second option)))
               (error 'measurement-error
                      :detail (format nil "tmux set-option ~A failed" (first option)))))
           (setf started t)
           server)
      (unless started (ignore-errors (stop-private-tmux server))))))

(defun private-tmux-attach-argv (server)
  (list (private-tmux-program server) "-L" (private-tmux-name server) "-f" "/dev/null"
        "attach-session" "-t" (private-tmux-session server)))

;;; Reference client: a second tmux client on a plain PTY. It records when the
;;; read completing each token arrived. This is output arrival, not display.

(defstruct reference pty thread (stop nil) (arrivals '())
  (lock (sb-thread:make-mutex :name "latency reference")))

(defun start-reference-client (server nonce slots &key (columns 80) (rows 24))
  (let* ((pty (lispterm.pty:open-pty :rows rows :columns columns))
         (reference (make-reference :pty pty))
         (screen (lispterm.engine:make-screen :rows rows :columns columns))
         (seen (make-hash-table)))
    (lispterm.pty:spawn-on-pty pty (private-tmux-attach-argv server)
                               :environment (cons "TERM=xterm-256color"
                                                  (remove-if (lambda (entry) (eql 0 (search "TERM=" entry)))
                                                             (private-tmux-environment server))))
    (setf (reference-thread reference)
          (sb-thread:make-thread
           (lambda ()
             (loop until (reference-stop reference)
                   do (let ((bytes (lispterm.pty:pty-read pty :timeout 0.05)))
                        (cond ((eq bytes :eof) (return))
                              (bytes
                               (let ((arrived (monotonic-ns)))
                                 (lispterm.engine:feed screen bytes)
                                 (dolist (index (screen-token-indexes nonce slots screen))
                                   (unless (gethash index seen)
                                     (setf (gethash index seen) t)
                                     (sb-thread:with-mutex ((reference-lock reference))
                                       (push (list index arrived)
                                             (reference-arrivals reference)))))))))))
           :name "latency reference client"))
    reference))

(defun stop-reference-client (reference)
  "Stop the reader, close the PTY and return the (index arrival-ns) records."
  (setf (reference-stop reference) t)
  (sb-thread:join-thread (reference-thread reference) :default nil :timeout 5)
  (lispterm.pty:close-pty (reference-pty reference) :terminate t)
  (sb-thread:with-mutex ((reference-lock reference))
    (reverse (reference-arrivals reference))))
