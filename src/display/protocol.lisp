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

(defvar *display-epoch-counter* 0)
(defvar *display-epoch-lock* (sb-thread:make-mutex :name "lispterm display epoch"))
(defun next-display-epoch ()
  (sb-thread:with-mutex (*display-epoch-lock*) (incf *display-epoch-counter*)))

(defstruct display
  (epoch (next-display-epoch))
  backend backend-state name title configuration font framebuffer damage
  (columns 80) (rows 24) (width 0) (height 0) (open-p nil)
  geometry-request (geometry-epoch 0) (presentation-generation 0) (metric-generation 0)
  (cleanup-pending-p nil)
  owner grid cursor-row cursor-col cursor-visible (reverse-screen nil) (blink-phase t))

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
          (record-loaded-font (display-font display) title)
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
    (cancel-size-request display)
    (setf (display-epoch display) (next-display-epoch))
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

;; Immutable request values are replaced, never modified after publication.
(defstruct (geometry-request (:copier nil))
  (id nil :read-only t) (epoch 0 :read-only t) (display-epoch 0 :read-only t)
  (columns 0 :read-only t) (rows 0 :read-only t)
  (width 0 :read-only t) (height 0 :read-only t)
  (cell-width 0 :read-only t) (cell-height 0 :read-only t)
  (deadline 0 :read-only t) (phase :pending :read-only t)
  (reason nil :read-only t) (presentation 0 :read-only t))
(defparameter *geometry-timeout* 2)
(defparameter *geometry-clock* #'get-internal-real-time)
(defun geometry-now () (funcall *geometry-clock*))
(define-condition geometry-incomplete (display-error)
  ((request :initarg :request :reader geometry-incomplete-request)))
(defgeneric backend-request-size (backend display request))
(defgeneric backend-cancel-size (backend display request))
(defmethod backend-cancel-size (backend display request)
  (declare (ignore backend display request)) nil)
(defmethod backend-request-size (backend display request)
  (declare (ignore backend))
  (geometry-result display request :refused :unsupported))

(defun geometry-result (display request phase &optional reason)
  "Replace an immutable value; obsolete epochs and terminal successes are ignored."
  (check-type request geometry-request)
  (let ((r (display-geometry-request display)))
    (when (and (geometry-current-p display request)
               (or (not (member phase '(:surface-ready :closing :settled)))
                   (member (geometry-request-phase r) '(:pending :surface-ready :closing))))
      (setf (display-geometry-request display)
            (make-geometry-request
             :id (geometry-request-id r) :epoch (geometry-request-epoch r)
             :display-epoch (geometry-request-display-epoch r)
             :columns (geometry-request-columns r) :rows (geometry-request-rows r)
             :width (geometry-request-width r) :height (geometry-request-height r)
             :cell-width (geometry-request-cell-width r) :cell-height (geometry-request-cell-height r)
             :deadline (geometry-request-deadline r) :phase phase :reason reason
             :presentation (if (eq phase :settled)
                               (display-presentation-generation display)
                               (geometry-request-presentation r)))))))

(defun geometry-current-p (display request)
  (let ((current (display-geometry-request display)))
    (and current (= (geometry-request-display-epoch request) (display-epoch display))
         (= (geometry-request-epoch request) (geometry-request-epoch current))
         (equal (geometry-request-id request) (geometry-request-id current)))))
(defun geometry-active-p (display)
  (let ((r (display-geometry-request display)))
    (and r (member (geometry-request-phase r) '(:pending :surface-ready :closing)))))
(defun cancel-size-request (display &optional (reason :cancelled))
  (assert-owner display)
  (let ((r (display-geometry-request display)))
    (when r
      (backend-cancel-size (display-backend display) display r)
      (geometry-result display r reason reason))))
(defun request-grid-size (display columns rows request-id)
  (assert-owner display)
  (check-type columns (integer 1 500))
  (check-type rows (integer 1 300))
  (cancel-size-request display :superseded)
  (incf (display-geometry-epoch display))
  (let* ((font (display-font display))
         (cw (font-width font)) (ch (font-height font))
         (r (make-geometry-request
             :id request-id :epoch (display-geometry-epoch display) :display-epoch (display-epoch display)
             :columns columns :rows rows :width (* columns cw) :height (* rows ch)
             :cell-width cw :cell-height ch
             :deadline (+ (geometry-now)
                          (ceiling (* *geometry-timeout* internal-time-units-per-second))))))
    (setf (display-geometry-request display) r)
    (backend-request-size (display-backend display) display r)
    r))
(defun geometry-tick (display)
  (let ((r (display-geometry-request display)))
    (when (and (geometry-active-p display)
               (or (/= (geometry-request-cell-width r) (font-width (display-font display)))
                   (/= (geometry-request-cell-height r) (font-height (display-font display)))))
      (incf (display-metric-generation display))
      (request-grid-size display (geometry-request-columns r) (geometry-request-rows r)
                         (geometry-request-id r))
      (return-from geometry-tick nil))
    (when (and (geometry-active-p display)
               (>= (geometry-now) (geometry-request-deadline r)))
      (backend-cancel-size (display-backend display) display r)
      (geometry-result display r :timed-out :deadline))))
(defun geometry-status (display)
  "Owner-produced portable snapshot; callers publish this under their own lock."
  (let ((r (display-geometry-request display)) (font (display-font display)))
    (list :request-id (and r (geometry-request-id r))
          :display-epoch (display-epoch display)
          :epoch (display-geometry-epoch display)
          :metric-generation (display-metric-generation display)
          :cell-width (and font (font-width font))
          :cell-height (and font (font-height font))
          :phase (if r (geometry-request-phase r) :idle)
          :reason (and r (geometry-request-reason r))
          :requested-columns (and r (geometry-request-columns r))
          :requested-rows (and r (geometry-request-rows r))
          :deadline (and r (geometry-request-deadline r))
          :requested-width (and r (geometry-request-width r))
          :requested-height (and r (geometry-request-height r))
          :actual-width (display-width display) :actual-height (display-height display)
          :columns (display-columns display) :rows (display-rows display)
          :presentation (display-presentation-generation display)
          :target-presentation (and r (geometry-request-presentation r)))))
(export '(request-grid-size cancel-size-request geometry-tick geometry-status
          geometry-active-p geometry-current-p geometry-request geometry-request-id geometry-request-width
          geometry-request-height geometry-result backend-request-size backend-cancel-size
          display-geometry-request display-presentation-generation resize-event-serial
          resize-event-states resize-event-scale resize-event-interactive-p))

(defun require-geometry-ready (display)
  "Owner-side recoverable check. Observers should inspect the published status."
  (assert-owner display)
  (let ((r (display-geometry-request display)))
    (when (and r (not (eq :settled (geometry-request-phase r))))
      (restart-case
          (error 'geometry-incomplete :request r :detail (geometry-status display))
        (retry () :report "Retry this grid request."
          (request-grid-size display (geometry-request-columns r) (geometry-request-rows r)
                             (geometry-request-id r)))
        (cancel () :report "Release this grid request."
          (cancel-size-request display)))))
  display)
(export '(geometry-incomplete geometry-incomplete-request require-geometry-ready))
