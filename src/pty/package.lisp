;;;; Linux x86-64 PTY transport. External programs use posix_spawnp, not Lisp fork.
(require :sb-posix)
(defpackage #:lispterm.pty
  (:nicknames #:common-term.pty)
  (:use #:cl)
  (:export #:open-pty #:pty #:pty-master-fd #:pty-slave-name #:pty-process #:pty-open-p
           #:pty-read #:pty-write #:pty-resize #:pty-window-size #:pty-foreground-process-group
           #:spawn-on-pty #:process #:process-pid #:process-status #:process-exit-code
           #:process-poll #:process-wait #:process-signal #:close-pty #:detach-pty #:attach-slave-to-stdio
           #:with-pty #:pty-error #:pty-open-error #:pty-io-error #:pty-spawn-error
           #:pty-closed-error #:stdio-rebind-refused #:pty-error-operation #:pty-error-errno))
(in-package #:lispterm.pty)
(define-condition pty-error (error)
  ((operation :initarg :operation :reader pty-error-operation)
   (errno :initarg :errno :initform nil :reader pty-error-errno))
  (:report (lambda (c s) (format s "PTY ~A failed~@[ (errno ~D)~]" (pty-error-operation c) (pty-error-errno c)))))
(define-condition pty-open-error (pty-error) ())
(define-condition pty-io-error (pty-error) ())
(define-condition pty-spawn-error (pty-error) ())
(define-condition pty-closed-error (pty-error) ())
(define-condition stdio-rebind-refused (pty-error) ())
(defstruct (pty (:constructor %make-pty)) master-fd slave-name process (open-p t)
  (lock (sb-thread:make-mutex :name "PTY transport"))
  ;; Never wait for transport ownership while holding this publication lock.
  (close-requested-p nil)
  (cancellation-lock (sb-thread:make-mutex :name "PTY cancellation")))
(defstruct (process (:constructor %make-process)) pid (status :running) exit-code
  (lock (sb-thread:make-mutex :name "PTY process")))
(defmacro with-pty ((variable &rest options) &body body)
  `(let ((,variable (open-pty ,@options)))
     (unwind-protect (progn ,@body) (close-pty ,variable :terminate t))))
