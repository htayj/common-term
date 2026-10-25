(in-package #:lispterm.pty)
(defun %c-string (value)
  (unless (and (stringp value) (not (find #\Null value)))
    (error 'type-error :datum value :expected-type 'string)) value)
(defun %spawn-check (code operation)
  (unless (zerop code) (%fail 'pty-spawn-error operation code)))
(defun %call-with-strings (strings function)
  (let ((pointers nil))
    (unwind-protect
         (progn
           (dolist (s strings) (push (cffi:foreign-string-alloc (%c-string s)) pointers))
           (setf pointers (nreverse pointers))
           (cffi:with-foreign-object (array :pointer (1+ (length pointers)))
             (loop for p in pointers for i from 0 do (setf (cffi:mem-aref array :pointer i) p))
             (setf (cffi:mem-aref array :pointer (length pointers)) (cffi:null-pointer))
             (funcall function array)))
      (dolist (p pointers) (cffi:foreign-string-free p)))))
(defun spawn-on-pty (pty argv &key (environment nil environment-p) directory)
  "Start external ARGV as session leader. ENVIRONMENT, when supplied, is a
list of NAME=VALUE strings (NIL means empty). Otherwise inherit environ."
  (unless (and (listp argv) argv (every #'stringp argv) (plusp (length (first argv))))
    (error 'type-error :datum argv :expected-type 'cons))
  (mapc #'%c-string argv)
  (when directory (%c-string directory))
  (when environment-p
    (unless (listp environment) (error 'type-error :datum environment :expected-type 'list))
    (dolist (entry environment)
      (%c-string entry)
      (unless (let ((equals (position #\= entry))) (and equals (plusp equals)))
        (error 'pty-spawn-error :operation :environment :errno nil))))
  (sb-thread:with-mutex ((pty-lock pty))
    (%ensure-open pty)
    (when (pty-process pty) (%fail 'pty-spawn-error :already-spawned nil))
    (cffi:with-foreign-objects ((actions '(:struct spawn-actions))
                                (attr '(:struct spawn-attributes)) (sigset :unsigned-long 16) (pid :int))
      (let ((actions-live nil) (attr-live nil))
        (unwind-protect
             (progn
               (%spawn-check (%actions-init actions) :actions-init) (setf actions-live t)
               (%spawn-check (%attributes-init attr) :attributes-init) (setf attr-live t)
               ;; glibc applies SETSID before file actions. Opening the slave
               ;; without O_NOCTTY makes it this new session's controlling tty.
               (%spawn-check (%actions-open actions 0 (pty-slave-name pty) sb-posix:o-rdwr 0) :actions-open)
               (%spawn-check (%actions-dup actions 0 1) :actions-dup)
               (%spawn-check (%actions-dup actions 0 2) :actions-dup)
               (when directory (%spawn-check (%actions-chdir actions directory) :actions-chdir))
               (%spawn-check (%sigempty sigset) :sigempty)
               (%spawn-check (%attributes-mask attr sigset) :signal-mask)
               (%spawn-check (%sigfill sigset) :sigfill)
               (%spawn-check (%attributes-defaults attr sigset) :signal-defaults)
               (%spawn-check (%attributes-flags attr (logior #x80 #x08 #x04)) :spawn-flags)
               (%call-with-strings argv
                 (lambda (args)
                   (flet ((spawn (env)
                            (%spawn-check (%spawnp pid (first argv) actions attr args env) :spawn)))
                     (if environment-p (%call-with-strings environment #'spawn) (spawn %environ)))))
               (setf (pty-process pty) (%make-process :pid (cffi:mem-ref pid :int))))
          (when attr-live (%attributes-destroy attr))
          (when actions-live (%actions-destroy actions)))))))
(defun process-poll (process)
  (sb-thread:with-mutex ((process-lock process))
    (when (eq (process-status process) :running)
      (cffi:with-foreign-object (status :int)
        (loop for rc = (%waitpid (process-pid process) status sb-posix:wnohang)
              do (cond ((zerop rc) (return))
                       ((plusp rc)
                        (let* ((word (cffi:mem-ref status :int)) (signal (logand word 127)))
                          (setf (process-status process) (if (zerop signal) :exited :signaled)
                                (process-exit-code process) (if (zerop signal) (ldb (byte 8 8) word) (+ 128 signal))))
                        (return))
                       ((= (%errno) sb-posix:eintr) (return))
                       (t (%fail 'pty-io-error :waitpid))))))
    (process-status process)))
(defun process-wait (process &key (timeout 5))
  (let ((deadline (%deadline timeout)))
    (loop for status = (process-poll process)
          unless (eq status :running) return status
          when (zerop (%remaining deadline)) return nil
          do (sleep (min 0.01 (%remaining deadline))))))
(defun process-signal (process signal)
  (check-type signal (integer 0 64))
  ;; Holding the reaper lock prevents PID reuse between checking and kill.
  (sb-thread:with-mutex ((process-lock process))
    (when (eq (process-status process) :running)
      (let ((rc (%kill (process-pid process) signal)))
        (when (and (minusp rc) (/= (%errno) sb-posix:esrch)) (%fail 'pty-io-error :kill)))))
  process)
(defun close-pty (pty &key (terminate t) (timeout 1))
  "Close the transport, optionally terminate and reap its child. With
TERMINATE NIL the caller retains responsibility for a still-running child."
  (%duration timeout)
  ;; Publish before waiting for the syscall lock. Polling owners and queued
  ;; readers/writers stop instead of repeatedly overtaking this closer.
  (%request-close pty)
  (let (process close-errno)
    (sb-thread:with-mutex ((pty-lock pty))
      ;; Sample the process under the same lock as spawn, so concurrent close
      ;; cannot overlook a child which has just been published.
      (setf process (pty-process pty))
      (when (pty-open-p pty)
        ;; Linux releases the fd even on EINTR; never retry close.
        (when (minusp (%close (pty-master-fd pty)))
          (let ((errno (%errno)))
            (unless (= errno sb-posix:eintr) (setf close-errno errno))))
        (setf (pty-open-p pty) nil (pty-master-fd pty) -1)))
    (when process
      (if terminate
          (progn
            (when (eq (process-poll process) :running)
              (process-signal process sb-posix:sighup))
            (unless (process-wait process :timeout timeout)
              (process-signal process sb-posix:sigkill)
              (unless (process-wait process :timeout 2)
                (%fail 'pty-io-error :reap-timeout nil))))
          (process-poll process)))
    (when close-errno (%fail (quote pty-io-error) :close close-errno)))
  t)

(defun detach-pty (pty)
  "Drop inherited transport and process ownership in a quiesced saver child.
All PTY/process users must be quiescent before fork; the child must have exclusive
access. Acquire no inherited locks and never signal, poll, or reap the process."
  (let ((fd (pty-master-fd pty))
        (open-p (pty-open-p pty))
        (process (pty-process pty)))
    ;; Invalidate first: Linux releases the descriptor even when close reports
    ;; EINTR. Fresh locks avoid retaining vanished-thread mutex ownership.
    (setf (pty-open-p pty) nil
          (pty-master-fd pty) -1
          (pty-process pty) nil
          (pty-close-requested-p pty) t
          (pty-cancellation-lock pty) (sb-thread:make-mutex :name "Detached PTY cancellation")
          (pty-lock pty) (sb-thread:make-mutex :name "Detached PTY transport"))
    (when process
      (setf (process-status process) :detached
            (process-pid process) nil
            (process-exit-code process) nil
            (process-lock process) (sb-thread:make-mutex :name "Detached PTY process")))
    (when (and open-p (minusp (%close fd)))
      (let ((errno (%errno)))
        (unless (= errno sb-posix:eintr)
          (%fail 'pty-io-error :detach-close errno)))))
  t)
