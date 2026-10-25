;;;; Source-backed integration with the installed Autolith TOOL runtime protocol.
(defpackage #:lispterm.autolith
  (:use #:cl)
  (:export #:register-runtime #:adopt-host #:forget-host #:runtime-status
           #:checkpoint-unavailable #:*runtime* #:*automatic-runtime*
           #:*automatic-main-thread* #:graphical-shell))
(in-package #:lispterm.autolith)

(define-condition checkpoint-unavailable (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "Graphical Autolith checkpoints are unavailable: stock checkpoint restoration cannot rebootstrap this PTY/window. Use source-backed startup." stream))))

(defstruct owned-host host hosted-main-p)
(defstruct (runtime (:constructor make-runtime ()))
  (hosts nil) (tokens nil) (state :running) (openings 0)
  (lock (sb-thread:make-mutex :name "lispterm Autolith runtime")))
(defvar *runtime* (make-runtime))
(defvar *automatic-runtime* nil)
(defvar *automatic-main-thread* nil)

(defun host-call (name &rest arguments)
  (apply #'uiop:symbol-call :lispterm name arguments))

(define-condition runtime-opening (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "A graphical shell is opening; retry checkpoint validation after it completes." stream))))

(defun open-owned-host (runtime &rest arguments)
  "Reserve lifecycle ownership before opening, then publish even recovery handles.
No host callback runs under the runtime mutex. A concurrent checkpoint refuses
before touching any host instead of waiting on a potentially reentrant factory."
  (let ((host nil))
    (sb-sys:without-interrupts
      (sb-thread:with-mutex ((runtime-lock runtime))
        (unless (eq (runtime-state runtime) :running)
          (error "Cannot open a host while runtime is ~S" (runtime-state runtime)))
        (incf (runtime-openings runtime)))
      (unwind-protect
           (handler-bind
               ((lispterm:host-error
                  (lambda (condition)
                    ;; OPEN retains a partial host when its own cleanup fails.
                    ;; Publish that recovery handle before releasing the reservation.
                    (when (lispterm:host-error-host condition)
                      (setf host (lispterm:host-error-host condition))))))
             (setf host (sb-sys:with-local-interrupts
                          (apply #'host-call :open arguments))))
        (sb-thread:with-mutex ((runtime-lock runtime))
          (when (and host (not (find host (runtime-hosts runtime)
                                    :key #'owned-host-host :test #'eq)))
            (push (make-owned-host :host host) (runtime-hosts runtime)))
          (decf (runtime-openings runtime)))))
    host))

(defun adopt-host (host &key hosted-main-p (runtime *runtime*))
  "Track HOST for shutdown and checkpoint validation; never redirect stdio."
  (sb-thread:with-mutex ((runtime-lock runtime))
    (unless (eq (runtime-state runtime) :running)
      (error "Cannot adopt a host while runtime is ~S" (runtime-state runtime)))
    (unless (find host (runtime-hosts runtime) :key #'owned-host-host :test #'eq)
      (push (make-owned-host :host host :hosted-main-p hosted-main-p)
            (runtime-hosts runtime))))
  host)

(defun forget-host (host &optional (runtime *runtime*))
  (sb-thread:with-mutex ((runtime-lock runtime))
    (setf (runtime-hosts runtime)
          (remove host (runtime-hosts runtime) :key #'owned-host-host :test #'eq)))
  nil)

(defun runtime-status (&optional (runtime *runtime*))
  (sb-thread:with-mutex ((runtime-lock runtime))
    (list :state (runtime-state runtime)
          :hosts (length (runtime-hosts runtime))
          :openings (runtime-openings runtime)
          :checkpoint-supported-p
          (notany #'owned-host-hosted-main-p (runtime-hosts runtime)))))

(defclass runtime-tool (autolith::tool)
  ((runtime :initarg :runtime :reader owner-runtime)))

(defmethod autolith::tool-runtime-identity ((tool runtime-tool))
  (owner-runtime tool))

(defmethod autolith::tool-execute ((tool runtime-tool) context arguments)
  (declare (ignore context arguments))
  (princ-to-string (runtime-status (owner-runtime tool))))

(defun resume-tokens (runtime)
  "Consume successful resumes so a failed resume is safely retryable."
  (loop while (runtime-tokens runtime)
        for pair = (first (runtime-tokens runtime))
        do (host-call :resume (car pair) (cdr pair))
           (pop (runtime-tokens runtime)))
  (setf (runtime-state runtime) :running)
  nil)

(defmethod autolith::tool-runtime-close ((tool runtime-tool))
  (let ((runtime (owner-runtime tool)))
    (sb-thread:with-mutex ((runtime-lock runtime))
      (when (plusp (runtime-openings runtime))
        (error 'runtime-opening))
      ;; A TOOL close participates in the pre-fork validation hook. Refuse before
      ;; changing anything, rather than save unusable primary terminal streams.
      (when (some #'owned-host-hosted-main-p (runtime-hosts runtime))
        (error 'checkpoint-unavailable))
      (unless (member (runtime-state runtime) '(:running :quiesced :detached))
        (error "Cannot close lispterm runtime while ~S; resume retained rollback tokens first"
               (runtime-state runtime)))
      (when (eq (runtime-state runtime) :running)
        (handler-case
            (progn
              (dolist (owned (runtime-hosts runtime))
                (let* ((host (owned-host-host owned))
                       (token (host-call :quiesce host)))
                  (push (cons host token) (runtime-tokens runtime))))
              (setf (runtime-state runtime) :quiesced))
          (serious-condition (cause)
            ;; The current host must roll back its own partial quiesce. We own
            ;; rollback of the hosts whose quiesce already returned a token.
            (setf (runtime-state runtime) :rollback-pending)
            (handler-case (resume-tokens runtime)
              (serious-condition (rollback)
                (error "Quiesce failed (~A); rollback also failed (~A). Tokens retained for retry."
                       cause rollback)))
            (error cause))))))
  nil)

(defmethod autolith::tool-runtime-resume
    ((tool runtime-tool) (registry autolith::tool-registry))
  (declare (ignore registry))
  (let ((runtime (owner-runtime tool)))
    (sb-thread:with-mutex ((runtime-lock runtime))
      (resume-tokens runtime)))
  nil)

(defmethod autolith::tool-runtime-detach ((tool runtime-tool))
  (let ((runtime (owner-runtime tool)))
    ;; Invoked only in the saver child after pre-fork joins. No X requests,
    ;; process signals, locks owned by vanished threads, or native destructors.
    (unless (member (runtime-state runtime) '(:quiesced :detached))
      (error "Cannot detach an unquiesced lispterm runtime"))
    (dolist (owned (runtime-hosts runtime))
      (host-call :detach-for-saver (owned-host-host owned)))
    (setf (runtime-hosts runtime) nil
          (runtime-tokens runtime) nil
          (runtime-state runtime) :detached))
  nil)

(defmethod autolith::tool-runtime-prune-checkpoint-state
    ((tool runtime-tool) (registry autolith::tool-registry))
  (remhash 'runtime-tool (autolith::tool-registry-runtime-bindings registry))
  (autolith::tool-registry-delete-if
   registry (lambda (candidate) (eq candidate tool)))
  nil)

(defun register-runtime (registry &optional (runtime *runtime*))
  "Install the owner in the actual application REGISTRY. Safe to repeat."
  (or (autolith::tool-registry-runtime-binding registry 'runtime-tool)
      (let ((tool (make-instance
                   'runtime-tool :runtime runtime :namespace "lispterm"
                   :name "status" :description "Inspect graphical terminal runtime state."
                   :parameters (autolith::json-object "type" "object"))))
        (autolith::tool-registry-register registry tool)
        (autolith::tool-registry-bind-runtime registry 'runtime-tool tool)
        tool)))

;; Autolith 0.67 has no extension factory callback. This narrowly scoped hook
;; runs only during standalone main, on its original thread. It does not modify
;; APPLICATION, TOOL, telemetry methods, or a tracked Autolith definition.
(defmethod initialize-instance :after
    ((registry autolith::tool-registry) &key)
  (when (and *automatic-runtime*
             (eq sb-thread:*current-thread* *automatic-main-thread*))
    (register-runtime registry *automatic-runtime*)))

(defun graphical-shell (application &key argv (backend :auto))
  "Open a fresh shell in this heap without touching primary stdio."
  (register-runtime (autolith::application-tool-registry application))
  (open-owned-host *runtime* :argv argv :backend backend))

(autolith::define-application-command graphical-shell-command
    (:name "/graphical-shell" :argument nil
     :description "open a separate graphical shell in this Lisp image"
     :tip "opens a fresh shell without replacing the primary terminal."
     :busy-behavior :execute :terminal-behavior :shared :callable t)
    (application)
  (let ((host (graphical-shell application)))
    (format *standard-output* "~&Graphical shell ~D~%" (getf (host-call :status host) :id)))
  :continue)
