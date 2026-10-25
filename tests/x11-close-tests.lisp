;;;; Exercise the real X11 backend close method without an X server.
(defpackage #:lispterm.x11-close-tests (:use #:cl))
(in-package #:lispterm.x11-close-tests)

(define-condition injected-close-error (error) ())

(defclass failing-close-stream (sb-gray:fundamental-binary-input-stream)
  ((transport :initarg :transport :reader transport)
   (failure :initarg :failure :reader failure)
   (fail-after-close :initarg :fail-after-close :reader fail-after-close)
   (attempts :initform 0 :accessor attempts)))

(defmethod open-stream-p ((stream failing-close-stream))
  (open-stream-p (transport stream)))

(defmethod close ((stream failing-close-stream) &key abort)
  (incf (attempts stream))
  (when (and (= 1 (attempts stream)) (not (fail-after-close stream)))
    (error (failure stream)))
  (close (transport stream) :abort abort)
  (when (= 1 (attempts stream)) (error (failure stream)))
  t)

(let ((checks 0))
  (flet ((check (value) (incf checks) (assert value)))
    ;; Cover failure before descriptor closure and failure after it. Retrying a
    ;; completed stream close must also be safe when cleanup was interrupted.
    (dolist (after '(nil t))
      (let* ((transport (open "/dev/null" :direction :io
                              :element-type '(unsigned-byte 8)
                              :if-exists :overwrite))
             (fd (sb-sys:fd-stream-fd transport))
             (condition (make-condition 'injected-close-error))
             (stream (make-instance 'failing-close-stream :transport transport
                                    :failure condition :fail-after-close after))
             (client (xclhb:make-client :stream stream))
             (state (lispterm.display.x11::make-x-state
                     :client client :window 123 :gc 456))
             (display (lispterm.display::make-display
                       :backend :x11 :backend-state state :open-p t
                       :owner sb-thread:*current-thread*)))
        (unwind-protect
             (progn
               (check (eq condition
                          (handler-case (lispterm.display:close-display display)
                            (injected-close-error (caught) caught))))
               (check (not (lispterm.display:display-open-p display)))
               (check (lispterm.display:display-cleanup-pending-p display))
               (check (eq state (lispterm.display:display-backend-state display)))
               (check (eq client (lispterm.display.x11::x-state-client state)))
               (check (eq stream (xclhb:client-stream client)))
               (check (= 123 (lispterm.display.x11::x-state-window state)))
               (check (= 456 (lispterm.display.x11::x-state-gc state)))
               (check (eq (not after) (open-stream-p transport)))
               (unless after (check (>= (sb-posix:fcntl fd sb-posix:f-getfd) 0)))
               ;; A nonowner must not advance the retry or discard ownership.
               (let ((result
                       (sb-thread:join-thread
                        (sb-thread:make-thread
                         (lambda ()
                           (handler-case (lispterm.display:close-display display)
                             (lispterm.display:display-error () :wrong-owner)))))))
                 (check (eq result :wrong-owner)))
               (check (= 1 (attempts stream)))
               (check (eq display (lispterm.display:close-display display)))
               (check (= 2 (attempts stream)))
               (check (not (open-stream-p transport)))
               (check (not (lispterm.display:display-cleanup-pending-p display)))
               (check (null (lispterm.display:display-backend-state display)))
               (check (handler-case (progn (sb-posix:fcntl fd sb-posix:f-getfd) nil)
                        (sb-posix:syscall-error (e)
                          (= sb-posix:ebadf (sb-posix:syscall-errno e)))))
               (check (eq display (lispterm.display:close-display display)))
               (check (= 2 (attempts stream))))
          (close transport :abort t)))))
  (format t "~&(:x11-close-checks ~d :passed T)~%" checks))
