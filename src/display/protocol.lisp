(in-package #:lispterm.display)

(define-condition display-error (error)
  ((detail :initarg :detail :reader display-error-detail))
  (:report (lambda (condition stream)
             (format stream "Display: ~a" (display-error-detail condition)))))
(define-condition display-open-failed (display-error)
  ((display :initarg :display :reader display-open-failed-display)
   (cleanup-error :initarg :cleanup-error :initform nil
                  :reader display-open-failed-cleanup-error)))

;; These accessors are part of the recovery interface, not backend internals.
(export '(display-cleanup-pending-p display-open-failed-display
          display-open-failed-cleanup-error))
(define-condition display-connection-lost (display-error) ())

(defstruct display
  backend backend-state name title configuration font framebuffer damage
  (columns 80) (rows 24) (width 0) (height 0) (open-p nil)
  (cleanup-pending-p nil)
  owner grid cursor-row cursor-col cursor-visible)

(defgeneric backend-open (backend display))
(defgeneric backend-close (backend display))
(defgeneric backend-poll (backend display timeout))
(defgeneric backend-blit (backend display framebuffer rectangles))
(defgeneric backend-set-title (backend display title))
(defgeneric backend-metrics (backend display))
(defmethod backend-metrics (backend display)
  (declare (ignore backend display))
  nil)

(defun assert-owner (display)
  (unless (eq (display-owner display) sb-thread:*current-thread*)
    (error 'display-error :detail "Display must be used on its owning thread")))

(declaim (ftype (function (t t t) t) resize-display))
(declaim (ftype (function (display) display) close-display))

(defun open-display (&key (backend :auto) display-name (title "Lispterm")
                          (columns 80) (rows 24) font-path (font-size 16))
  (check-type columns (integer 1 10000))
  (check-type rows (integer 1 10000))
  (let* ((backend (if (eq backend :auto)
                      (if (uiop:getenv "WAYLAND_DISPLAY") :wayland :x11)
                      backend))
         (display
           (make-display
            :backend backend :name display-name :title title
            :columns columns :rows rows :owner sb-thread:*current-thread*
            :configuration (list :backend backend :display-name display-name
                                 :title title :columns columns :rows rows
                                 :font-path font-path :font-size font-size))))
    (handler-case
        (progn
          (setf (display-font display) (load-font font-path font-size))
          (resize-display display
                          (* columns (font-width (display-font display)))
                          (* rows (font-height (display-font display))))
          ;; Retain cleanup responsibility even if opening is interrupted.
          (setf (display-cleanup-pending-p display) t)
          ;; Broken local transports must not hold the caller indefinitely.
          (sb-ext:with-timeout 10 (backend-open backend display))
          (setf (display-open-p display) t
                (display-cleanup-pending-p display) nil)
          display)
      (error (condition)
        (let ((cleanup-error
                (handler-case (progn (close-display display) nil)
                  (error (cleanup-condition) cleanup-condition))))
          (error 'display-open-failed :detail condition :display display
                 :cleanup-error cleanup-error))))))

(defun close-display (display)
  "Close on the owning thread. A failed cleanup leaves DISPLAY unusable and
cleanup pending; retry this function before declaring quiescence or reopening.
Backend cleanup must be retryable and retain unfinished resource ownership."
  (assert-owner display)
  (when (or (display-open-p display) (display-cleanup-pending-p display))
    ;; Do not claim that a partially closed transport can still be used.
    ;; Leave the pending flag set on every nonlocal exit, including a timeout.
    (setf (display-cleanup-pending-p display) t
          (display-open-p display) nil)
    (backend-close (display-backend display) display)
    (setf (display-cleanup-pending-p display) nil))
  display)

(defun reopen-display (display)
  (assert-owner display)
  (close-display display)
  (apply #'open-display (display-configuration display)))

(defun poll-events (display &key (timeout 0))
  (assert-owner display)
  (check-type timeout (real 0))
  (when (display-open-p display)
    (handler-case
        (sb-ext:with-timeout (+ timeout 0.25)
          (backend-poll (display-backend display) display timeout))
      (sb-ext:timeout ()
        (error 'display-connection-lost :detail "Display transport poll stalled")))))

(defun present (display)
  (assert-owner display)
  (when (and (display-open-p display) (display-damage display))
    (backend-blit (display-backend display) display
                  (display-framebuffer display) (display-damage display))
    (setf (display-damage display) nil))
  display)

(defun set-title (display title)
  (assert-owner display)
  (when (display-cleanup-pending-p display)
    (error 'display-error :detail "Display cleanup is pending"))
  (setf (display-title display) title)
  (backend-set-title (display-backend display) display title))

(defun display-metrics (display)
  (when (display-cleanup-pending-p display)
    (error 'display-error :detail "Display cleanup is pending"))
  (append (list :columns (display-columns display) :rows (display-rows display)
                :width (display-width display) :height (display-height display)
                :cell-width (font-width (display-font display))
                :cell-height (font-height (display-font display))
                :backend (display-backend display))
          (backend-metrics (display-backend display) display)))
