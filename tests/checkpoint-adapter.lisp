;;;; Actual Autolith backend validation and TOOL lifecycle, in a disposable image.
(load "scripts/bootstrap.lisp")
(asdf:load-system "autolith")
(asdf:load-system "lispterm")
(load "src/autolith-adapter.lisp")
(defpackage #:lispterm.checkpoint-tests (:use #:cl))
(in-package #:lispterm.checkpoint-tests)
(defvar *checks* 0)
(defun check (value label)
  (unless value (error "Checkpoint check failed: ~A" label))
  (incf *checks*)
  (format t "CHECK ~D ~A~%" *checks* label))
(defun shell-host ()
  (lispterm:open :backend (intern (string-upcase (or (uiop:getenv "LISPTERM_TEST_BACKEND") "wayland")) :keyword)
                 :argv '("/bin/sh") :rows 20 :columns 70 :title "Lispterm checkpoint validation"))
(defun await-output (host before)
  (loop repeat 100
        when (> (getf (lispterm:status host) :bytes-received) before) return t
        do (sleep .05)))
(defun make-backend (registry)
  (autolith::checkpoint-backend-create
   (autolith::configuration-create :defer-provider-validation-p t) nil :tool-registry registry))
(defun validate (backend)
  ;; Actual installed validation callback, including source revalidation and the
  ;; secret-use check. No mock backend and no primary checkpoint operation.
  (let* ((configuration (autolith::configuration-create :defer-provider-validation-p t))
         (source (autolith::make-source-provenance (autolith::config :source-root configuration)))
         (revision (autolith::source-revision source)))
    (funcall (sbcl-generations::checkpoint-backend-validate-function backend) revision)))
(defun resume-backend (backend)
  (funcall (sbcl-generations::checkpoint-backend-resume-function backend) nil))
(defclass failing-tool (autolith::tool) ())
(defmethod autolith::tool-runtime-identity ((tool failing-tool)) tool)
(defmethod autolith::tool-runtime-close-priority ((tool failing-tool)) -10)
(defmethod autolith::tool-runtime-close ((tool failing-tool))
  (declare (ignore tool))
  (error "Injected later-runtime close failure"))

(defun run ()
  (let* ((runtime (lispterm.autolith::make-runtime))
         (registry (make-instance 'autolith::tool-registry))
         (owner (lispterm.autolith:register-runtime registry runtime))
         (first (shell-host)) (second nil))
    (unwind-protect
         (progn
           (setf second (shell-host))
           (lispterm.autolith:adopt-host second :runtime runtime)
           (lispterm.autolith:adopt-host first :runtime runtime)
           (check (eq owner (lispterm.autolith:register-runtime registry runtime)) "registration is idempotent")
           (check (eq runtime (autolith::tool-runtime-identity owner)) "registry identity is shared runtime")
           ;; Use actual validation close/resume closures with two real windows.
           (let ((backend (make-backend registry)))
             (validate backend)
             (check (not (getf (lispterm:status first) :worker-alive-p)) "validation joined first window owner")
             (check (not (getf (lispterm:status second) :worker-alive-p)) "validation joined second window owner")
             (check (lispterm.pty:pty-open-p (lispterm::session-pty (lispterm::host-session first))) "parent master retained before fork")
             (resume-backend backend)
             (check (eq :attached (getf (lispterm:status first) :state)) "parent window resumed")
             (resume-backend backend)
             (check (getf (lispterm:status first) :worker-alive-p) "resume is idempotent"))
           ;; Failure after this owner succeeded must recover through the actual
           ;; backend's retained successful-tool list.
           (let ((failure (make-instance 'failing-tool :namespace "test" :name "fail"
                                        :description "Injected validation failure"
                                        :parameters (autolith::json-object "type" "object")))
                 (backend (make-backend registry)))
             (autolith::tool-registry-register registry failure)
             (unwind-protect
                  (check (handler-case (progn (validate backend) nil) (error () t)) "later validation close fails")
               (resume-backend backend)
               (autolith::tool-registry-delete-if registry (lambda (tool) (eq tool failure))))
             (check (eq :attached (getf (lispterm:status first) :state)) "failed validation resumed successful close"))
           ;; Partial close: first real host quiesces, then another host fails.
           ;; The owner must roll back its successes before signalling.
           (let ((original (symbol-function 'lispterm.autolith::host-call)))
             (unwind-protect
                  (progn
                    (setf (symbol-function 'lispterm.autolith::host-call)
                          (lambda (name &rest arguments)
                            (when (and (eq name :quiesce) (eq (first arguments) second))
                              (error "Injected second-host quiesce failure"))
                            (apply original name arguments)))
                    (check (handler-case (progn (autolith::tool-runtime-close owner) nil) (error () t)) "partial owner close fails"))
               (setf (symbol-function 'lispterm.autolith::host-call) original)))
           (check (and (eq :attached (getf (lispterm:status first) :state))
                       (eq :running (getf (lispterm.autolith:runtime-status runtime) :state))) "partial owner close self-rolls back")
           ;; A failed rollback retains tokens and must not report a successful
           ;; close on retry. Explicit resume can recover after the cause clears.
           (let ((original (symbol-function 'lispterm.autolith::host-call)))
             (unwind-protect
                  (progn
                    (setf (symbol-function 'lispterm.autolith::host-call)
                          (lambda (name &rest arguments)
                            (when (or (and (eq name :quiesce) (eq (first arguments) second))
                                      (eq name :resume))
                              (error "Injected close/rollback failure"))
                            (apply original name arguments)))
                    (check (handler-case (progn (autolith::tool-runtime-close owner) nil) (error () t)) "double failure is reported")
                    (check (eq :rollback-pending (getf (lispterm.autolith:runtime-status runtime) :state)) "failed rollback retains pending state")
                    (check (handler-case (progn (autolith::tool-runtime-close owner) nil) (error () t)) "pending rollback blocks another close"))
               (setf (symbol-function 'lispterm.autolith::host-call) original)))
           (autolith::tool-runtime-resume owner registry)
           (check (eq :attached (getf (lispterm:status first) :state)) "retained token recovers after rollback failure clears")
           ;; Enforce the advertised unsupported hosted-main checkpoint boundary
           ;; through the real backend validation path, before any fork.
           (setf (lispterm.autolith::owned-host-hosted-main-p
                  (find first (lispterm.autolith::runtime-hosts runtime)
                        :key #'lispterm.autolith::owned-host-host)) t)
           (let ((backend (make-backend registry)))
             (unwind-protect
                  (check (handler-case (progn (validate backend) nil)
                           (lispterm.autolith:checkpoint-unavailable () t)) "hosted-main validation explicitly refused")
               (resume-backend backend)))
           (check (eq :attached (getf (lispterm:status first) :state)) "refusal kept primary terminal running")
           (setf (lispterm.autolith::owned-host-hosted-main-p
                  (find first (lispterm.autolith::runtime-hosts runtime)
                        :key #'lispterm.autolith::owned-host-host)) nil)
           ;; Cross an actual fork boundary with no lispterm workers alive. Child
           ;; detach closes inherited handles only; external shells belong to us.
           (let ((backend (make-backend registry)))
             (validate backend)
             (unwind-protect
                  (let ((pid (sb-posix:fork)))
                    (if (zerop pid)
                        (handler-case
                            (progn
                              (autolith::tool-registry-detach-runtime-state registry)
                              (unless (and (eq :saved (getf (lispterm:status first) :state))
                                           (not (lispterm.pty:pty-open-p (lispterm::session-pty (lispterm::host-session first))))
                                           (null (autolith::tool-registry-find registry "lispterm" "status"))
                                           (null (lispterm.autolith::runtime-hosts runtime)))
                                (error "Child detach retained a runtime root"))
                              (cffi:foreign-funcall "_exit" :int 0 :void))
                          (serious-condition () (cffi:foreign-funcall "_exit" :int 3 :void)))
                        (multiple-value-bind (waited status) (sb-posix:waitpid pid 0)
                          (check (and (= waited pid) (zerop status)) "forked saver detached handles and pruned roots"))))
               (resume-backend backend)))
           (let ((before (getf (lispterm:status first) :bytes-received)))
             (lispterm:send-input first (format nil "printf 'PARENT-AFTER-DETACH\\n'~C" #\Return))
             (check (await-output first before) "parent shell accepts input after saver detach"))
           (check (eq :attached (getf (lispterm:status first) :state)) "parent graphical window remains usable")
           (format t "CHECKPOINT TOOL GATE PASS ~D checks; graphical core restore explicitly unavailable~%" *checks*))
      (dolist (host (remove nil (list first second)))
        (ignore-errors (lispterm:close host))))))
(run)
