;;;; Optional controls of the private tmux helper. tmux is a secondary comparison
;;;; workload, so these run only on request: tests/verify.sh
;;;; latency-measurement-controls --tmux. Starts one private tmux server and removes it.
;;;; Load after tests/latency-tmux.lisp and tests/latency-measurement-controls.lisp.
(in-package #:lispterm.latency.controls)

(defun missing-tmux-controls ()
  "A missing prerequisite must fail before attempting a temporary directory."
  (let ((program (symbol-function 'tmux-program))
        (mkdtemp (symbol-function 'sb-posix:mkdtemp))
        (directory-attempts 0))
    (unwind-protect
         (sb-ext:without-package-locks
           (setf (symbol-function 'tmux-program)
                 (lambda () (error 'measurement-error :detail "stub: tmux is absent"))
                 (symbol-function 'sb-posix:mkdtemp)
                 (lambda (template)
                   (incf directory-attempts)
                   template))
           (check :missing-tmux-signals-measurement-error
                  (signals-measurement-error-p
                   (lambda () (start-private-tmux '("unused")))))
           (check :missing-tmux-does-not-create-directory (zerop directory-attempts)))
      (sb-ext:without-package-locks
        (setf (symbol-function 'tmux-program) program
              (symbol-function 'sb-posix:mkdtemp) mkdtemp)))))

(defun cleanup-controls ()
  (let ((version (tmux-version)))
    (format t "~S~%" (list :tmux :program (tmux-program) :version version))
    (check :tmux-version-recorded (plusp (length version))))
  (let* ((server (start-private-tmux '("sleep" "60")))
         (directory (private-tmux-directory server)))
    (unwind-protect
         (progn
           (check :private-tmux-runs (private-tmux-running-p server))
           (check :private-tmux-socket-is-under-its-own-directory
                  (and (probe-file (private-tmux-socket server))
                       (eql 0 (search directory (private-tmux-socket server)))))
           (check :live-server-is-visible-to-the-cleanup-check
                  (and (probe-file (format nil "~A/" directory)) (private-tmux-running-p server))))
      (stop-private-tmux server))
    (check :private-tmux-stopped (not (private-tmux-running-p server)))
    (check :private-tmux-directory-removed (null (probe-file (format nil "~A/" directory))))))

(setf *groups* (append (remove-if (lambda (group)
                                  (member group '(missing-tmux-controls cleanup-controls)))
                                *groups*)
                       '(missing-tmux-controls cleanup-controls)))
