(defpackage #:lispterm
  (:use #:cl)
  (:shadow #:open #:close)
  (:export #:open #:close #:main #:lispterm-error
           #:host #:status #:reopen #:quiesce #:resume #:detach-for-saver
           #:host-error #:host-error-host #:host-timeout #:host-errors #:invalidate #:send-input
           #:input-backpressure #:input-backpressure-pending #:input-backpressure-requested
           #:input-cancelled #:input-cancelled-bytes))
(defpackage #:lispterm.engine.compat
  (:use #:cl)
  (:export #:char-width #:decode-error #:lock #:make-lock #:with-lock-held
           #:octets-to-string #:string-to-octets #:parse-integer-or-nil
           #:split-string #:version-string))
(defpackage #:lispterm.engine
  (:use #:cl)
  (:shadow #:make-screen #:resize #:rows #:columns)
  (:export #:make-screen #:feed #:resize #:rows #:columns #:cursor-row
           #:cursor-column #:cell-at #:cell-character #:cell-width
           #:cell-foreground #:cell-background #:cell-bold-p #:cell-inverse-p
           #:cell-underline-p #:cell-blink-p #:line-size #:logical-columns #:reverse-screen-p
           #:dirty-p #:clear-damage #:palette-color))

;;; Opt-in stage tracing for latency measurement, shared by the display,
;;; session and window layers. Nothing is timed or allocated while the hook is NIL.
(defpackage #:lispterm.trace
  (:use #:cl)
  (:export #:*trace* #:*trace-failure* #:trace-event #:monotonic-ns))
(in-package #:lispterm.trace)
(defvar *trace* nil
  "NIL, or (STAGE TIME-NS &rest PLIST), called synchronously on the reporting
thread, sometimes inside session/protocol critical sections. Hooks must not
reenter production operations, block indefinitely, mutate or retain live objects,
or transfer control out using THROW/GO/RETURN-FROM. Ordinary ERRORs are contained;
arbitrary nonlocal exits cannot be contained portably. Use ERROR to abort a hook.
Set the global value for owner-thread tracing. Retire and drain reporters before
snapshotting. Replacing this variable alone does not quiesce an entered hook.")
(defvar *trace-failure* nil
  "Sticky failure evidence from the trace boundary. Reset only before installing
a hook, then check after draining it. Non-NIL invalidates the measurement. When
reporters fail concurrently, which failure is retained is unspecified.")
(defvar *reporting-trace* nil)

;; Serialize admission with retirement, not the reporting work. The disabled
;; macro path never touches this state. Replacing *TRACE* directly is not drain.
(defvar *trace-boundary-lock* (sb-thread:make-mutex :name "trace boundary"))
(defvar *trace-boundary-drained* (sb-thread:make-waitqueue :name "trace boundary"))
(defvar *trace-boundary-active* 0)
(defun admit-trace-boundary ()
  (sb-thread:with-mutex (*trace-boundary-lock*)
    (when *trace*
      (incf *trace-boundary-active*)
      *trace*)))
(defun leave-trace-boundary ()
  (sb-thread:with-mutex (*trace-boundary-lock*)
    (decf *trace-boundary-active*)
    (when (zerop *trace-boundary-active*)
      (sb-thread:condition-broadcast *trace-boundary-drained*))))
(defun retire-trace-boundary ()
  "Disable global tracing and drain clock, arguments and hook invocations."
  (when *reporting-trace* (error "Cannot retire tracing from its reporter"))
  (sb-thread:with-mutex (*trace-boundary-lock*)
    (setf *trace* nil)
    (loop while (plusp *trace-boundary-active*)
          do (sb-thread:condition-wait *trace-boundary-drained* *trace-boundary-lock*))))
(declaim (inline monotonic-ns))
(defun monotonic-ns ()
  "CLOCK_MONOTONIC in integer nanoseconds."
  (multiple-value-bind (seconds nanoseconds)
      (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* seconds 1000000000) nanoseconds)))
(defmacro trace-event (stage &rest plist)
  "Report STAGE. Disabled tracing evaluates neither clock nor argument forms.
Contain ordinary errors, suppress recursive reports and retain failure evidence.
The hook contract forbids arbitrary nonlocal control transfer and production
reentry; suppressing nested trace calls cannot make arbitrary callbacks safe."
  (let ((hook (gensym "HOOK")))
    `(when *trace*
       (let ((,hook (admit-trace-boundary)))
         (when ,hook
           (unwind-protect
                (if *reporting-trace*
                    (setf *trace-failure* :recursive-trace)
                    (let ((*reporting-trace* t))
                      (handler-case
                          (funcall ,hook ,stage (monotonic-ns) ,@plist)
                        (error (condition)
                          (setf *trace-failure* condition)))))
             (leave-trace-boundary)))))))
