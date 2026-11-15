;;;; Offline: scripts/sbcl --script tests/trace-safety-controls.lisp [--baseline]
(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system "lispterm")
(defparameter *trace-baseline* (member "--baseline" sb-ext:*posix-argv* :test #'string=))
(when *trace-baseline*
  (load (merge-pathnames "../../trace-safety-preimages/package.lisp" *load-truename*))
  ;; Recompile actual critical boundaries with the old macro, not merely a model.
  (load (merge-pathnames "../src/session.lisp" *load-truename*))
  (load (merge-pathnames "../src/display/wayland-shm.lisp" *load-truename*)))
(load (merge-pathnames "latency-tokens.lisp" *load-truename*))
(load (if *trace-baseline*
          (merge-pathnames "../../trace-safety-preimages/latency-measure.lisp" *load-truename*)
          (merge-pathnames "latency-measure.lisp" *load-truename*)))

;; Reproduce the reviewed 131 interleaving in disposable processes: its actual
;; macro and its clear-without-boundary-drain cleanup. Recorder code is identical.
(when (member "--boundary-baseline" sb-ext:*posix-argv* :test #'string=)
  (eval '(defmacro lispterm.trace:trace-event (stage &rest plist)
           (let ((hook (gensym "HOOK")))
             `(let ((,hook lispterm.trace:*trace*))
                (when ,hook
                  (if lispterm.trace::*reporting-trace*
                      (setf lispterm.trace:*trace-failure* :recursive-trace)
                      (let ((lispterm.trace::*reporting-trace* t))
                        (handler-case
                            (funcall ,hook ,stage (lispterm.trace:monotonic-ns) ,@plist)
                          (error (condition)
                            (setf lispterm.trace:*trace-failure* condition))))))))))
  (setf (symbol-function 'lispterm.trace::retire-trace-boundary)
        (lambda () (setf lispterm.trace:*trace* nil))))
(defpackage #:lispterm.trace.controls (:use #:cl))
(in-package #:lispterm.trace.controls)
(defvar *checks* 0)
(defvar *failures* 0)
(defun check (name value)
  (incf *checks*)
  (unless value (incf *failures*))
  (format t "~A ~A~%" (if value "PASS" "FAIL") name))
(defmacro safely (&body body) `(handler-case (progn ,@body) (error () nil)))
(defmacro replacing ((symbol function) &body body)
  `(let ((old (symbol-function ,symbol)))
     (unwind-protect (progn (setf (symbol-function ,symbol) ,function) ,@body)
       (setf (symbol-function ,symbol) old))))
(defun hook-error (&rest arguments) (declare (ignore arguments)) (error "recorder fault"))

(let ((lispterm.trace:*trace* nil) (evaluated 0))
  (lispterm.trace:trace-event (incf evaluated) :value (incf evaluated))
  (check :disabled-no-evaluation (zerop evaluated)))
(flet ((lispterm.trace:monotonic-ns () (error "clock evaluated")))
  (let ((lispterm.trace:*trace* nil))
    (check :disabled-no-clock (safely (lispterm.trace:trace-event :unused) t)))
  (let ((lispterm.trace:*trace* #'hook-error))
    (check :clock-error-contained (safely (lispterm.trace:trace-event :clock) t))))
(let ((lispterm.trace:*trace* #'hook-error))
  (setf lispterm.trace:*trace-failure* nil)
  (check :ordinary-error-contained (safely (lispterm.trace:trace-event :failure) t))
  (check :error-evidence lispterm.trace:*trace-failure*)
  (check :argument-error-contained
         (safely (lispterm.trace:trace-event :argument :value (error "argument")) t))
  (let ((original (make-condition 'simple-error :format-control "original")))
    (check :cleanup-does-not-mask-original
           (eq original (handler-case
                            (unwind-protect (error original)
                              (lispterm.trace:trace-event :present-result))
                          (error (condition) condition))))))
(let ((calls 0))
  (setf lispterm.trace:*trace-failure* nil)
  (let ((lispterm.trace:*trace*
          (lambda (stage &rest args)
            (declare (ignore args)) (incf calls)
            (when (eq stage :outer) (lispterm.trace:trace-event :inner)))))
    (lispterm.trace:trace-event :outer))
  (check :recursive-report-suppressed (= calls 1))
  (check :recursive-report-invalidates lispterm.trace:*trace-failure*))

;; Actual FIFO advancement after successful accepted-prefix write.
(replacing ('lispterm.pty:pty-write (lambda (pty bytes &key timeout)
                                    (declare (ignore pty timeout)) (length bytes)))
  (let ((session (lispterm::make-session nil 2 8)))
    (lispterm::session-write session
                             (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(65 66)))
    (let ((lispterm.trace:*trace* #'hook-error))
      (safely (lispterm::session-flush-input session)))
    (check :actual-queue-advances (null (lispterm::session-input-head session)))
    (check :actual-queue-offset-reset (zerop (lispterm::session-input-offset session)))))

;; Actual SHM present and callback, only wire transport replaced. No socket/GUI.
(replacing ('lispterm.display.wayland::wire-send
            (lambda (&rest arguments) (declare (ignore arguments)) nil))
  (cffi:with-foreign-object (pixels :uint32 1)
    (let* ((wire (lispterm.display.wayland::%make-wire))
           (buffer (lispterm.display.wayland::make-shm-buffer
                    :id 77 :pointer pixels :size 4 :width 1 :height 1 :generation 0))
           (state (lispterm.display.wayland::make-wayland-state
                   :wire wire :configured-p t :width 1 :height 1 :buffers (list buffer))))
      (let ((lispterm.trace:*trace*
              (lambda (stage &rest args)
                (declare (ignore args)) (when (eq stage :commit) (error "commit hook")))))
        (safely (lispterm.display.wayland::protocol-present state #(42) 1 1)))
      (check :actual-commit-bookkeeping
             (= 1 (lispterm.display.wayland::wayland-state-commit-generation state)))
      (check :actual-frame-pending
             (lispterm.display.wayland::wayland-state-frame-pending-p state))
      ;; Test the callback boundary independently of the commit-boundary failure.
      (setf (lispterm.display.wayland::wayland-state-frame-pending-p state) t)
      (let ((callback (gethash 2 (lispterm.display.wayland::wire-handlers wire))))
        (when callback
          (let ((lispterm.trace:*trace* #'hook-error)) (safely (funcall callback 2 0 #())))))
      (check :actual-callback-clears-pending
             (and (= 1 (lispterm.display.wayland::wayland-state-frame-generation state))
                  (not (lispterm.display.wayland::wayland-state-frame-pending-p state)))))))

(let ((recorder (lispterm.latency:make-recorder "SAFE" 4 :audit #'hook-error)))
  (check :audit-error-contained
         (safely (funcall (lispterm.latency:recorder-hook recorder) :copy 1 :store nil) t))
  (check :audit-error-rejects-snapshot
         (handler-case (progn (lispterm.latency:recorded-events recorder) nil)
           (lispterm.latency:measurement-error () t))))
(check :macro-failure-rejects-collection
       (handler-case
           (progn (lispterm.latency:call-with-recorder
                   (lispterm.latency:make-recorder "SAFE" 4)
                   (lambda () (lispterm.trace:trace-event :bad :value (error "argument")))) nil)
         (lispterm.latency:measurement-error () t)
         (error () nil)))

(let* ((recorder (lispterm.latency:make-recorder "SAFE" 4))
       (original (make-condition 'simple-error :format-control "body failure")))
  (check :workload-error-not-masked
         (eq original
             (handler-case
                 (lispterm.latency:call-with-recorder recorder (lambda () (error original)))
               (error (condition) condition)))))
(let* ((calls 0) (hook nil)
       (recorder (lispterm.latency:make-recorder
                  "SAFE" 4 :audit (lambda (&rest args) (declare (ignore args))
                                   (when (= 1 (incf calls)) (funcall hook :copy 2 :store nil)) nil))))
  (setf hook (lispterm.latency:recorder-hook recorder))
  (funcall hook :copy 1 :store nil)
  (check :direct-recorder-recursion-suppressed (= 1 calls))
  (check :direct-recorder-recursion-rejects
         (handler-case (progn (lispterm.latency:recorded-events recorder) nil)
           (lispterm.latency:measurement-error () t))))
;; An arbitrary THROW is outside the supported boundary. It escapes, but an
;; entered recorder must still drain and reject its incomplete evidence.
(let ((recorder (lispterm.latency:make-recorder
                 "SAFE" 4 :audit (lambda (&rest args) (declare (ignore args)) (throw 'outside :escaped)))))
  (check :arbitrary-throw-is-not-claimed-contained
         (eq :escaped (catch 'outside
                        (funcall (lispterm.latency:recorder-hook recorder) :copy 1 :store nil))))
  (check :nonlocal-exit-evidence-rejected
         (handler-case (progn (lispterm.latency:recorded-events recorder) nil)
           (lispterm.latency:measurement-error () t))))

;; Pause the actual macro BEFORE recorder admission, during a fallible clock or
;; argument. Retirement must wait, reject the failure, and isolate the next run.
(defun boundary-retirement-control (location)
  (let* ((entered (sb-thread:make-semaphore)) (release (sb-thread:make-semaphore))
         (retiring (sb-thread:make-semaphore)) (done (sb-thread:make-semaphore))
         (next-entered (sb-thread:make-semaphore)) (next-release (sb-thread:make-semaphore))
         (fault (make-condition 'simple-error :format-control "paused boundary failure"))
         (reporter nil) (first-result nil) (first-failure nil) (next-result nil)
         (retire (symbol-function 'lispterm.trace::retire-trace-boundary)))
    (flet ((pause ()
             (sb-thread:signal-semaphore entered)
             (sb-thread:wait-on-semaphore release)
             (error fault))
           (collect (body)
             (handler-case
                 (progn (lispterm.latency:call-with-recorder
                         (lispterm.latency:make-recorder "SAFE" 4) body) :complete)
               (lispterm.latency:measurement-error () :rejected))))
      (replacing ('lispterm.trace::retire-trace-boundary
                  (lambda () (sb-thread:signal-semaphore retiring) (funcall retire)))
        (let ((collector
                (sb-thread:make-thread
                 (lambda ()
                   (setf first-result
                         (collect
                          (lambda ()
                            (setf reporter
                                  (sb-thread:make-thread
                                   (lambda ()
                                     (ecase location
                                       (:argument (lispterm.trace:trace-event :paused :value (pause)))
                                       (:clock (flet ((lispterm.trace:monotonic-ns () (pause)))
                                                 (lispterm.trace:trace-event :paused)))))))
                            (sb-thread:wait-on-semaphore entered))))
                   (setf first-failure lispterm.trace:*trace-failure*)
                   (sb-thread:signal-semaphore done)))))
          (sb-thread:wait-on-semaphore retiring)
          (let ((early (sb-thread:wait-on-semaphore done :timeout .05)))
            (check (list location :boundary-retirement-waits) (not early))
            ;; On the old boundary, let the next collection reset the sticky
            ;; failure before releasing the previous reporter. This reproduces
            ;; contamination, not just an error noticed after a false receipt.
            (unless early
              (sb-thread:signal-semaphore release)
              (sb-thread:join-thread reporter)
              (sb-thread:join-thread collector))
            (let ((next (sb-thread:make-thread
                         (lambda ()
                           (setf next-result
                                 (collect (lambda ()
                                            (sb-thread:signal-semaphore next-entered)
                                            (sb-thread:wait-on-semaphore next-release)
                                            (lispterm.trace:trace-event :next))))))))
              (sb-thread:wait-on-semaphore next-entered)
              (when early
                (sb-thread:signal-semaphore release)
                (sb-thread:join-thread reporter)
                (sb-thread:join-thread collector))
              (sb-thread:signal-semaphore next-release)
              (sb-thread:join-thread next))
            (check (list location :failed-boundary-never-complete) (eq first-result :rejected))
            (check (list location :sticky-original-failure) (eq first-failure fault))
            (check (list location :next-collection-isolated) (eq next-result :complete))))))))
(unless cl-user::*trace-baseline*
  (boundary-retirement-control :argument)
  (boundary-retirement-control :clock))
;; Hold an admitted reporter inside its audit, before the old event-list lock.
(let* ((entered (sb-thread:make-semaphore)) (release (sb-thread:make-semaphore))
       (starting (sb-thread:make-semaphore)) (done (sb-thread:make-semaphore))
       (recorder (lispterm.latency:make-recorder
                  "SAFE" 4 :audit (lambda (&rest args) (declare (ignore args))
                                   (sb-thread:signal-semaphore entered)
                                   (sb-thread:wait-on-semaphore release) nil)))
       (hook (lispterm.latency:recorder-hook recorder)) (result nil)
       (reporter (sb-thread:make-thread (lambda () (funcall hook :copy 1 :store nil)))))
  (sb-thread:wait-on-semaphore entered)
  (let ((snapshot (sb-thread:make-thread
                   (lambda () (sb-thread:signal-semaphore starting)
                     (setf result (lispterm.latency:recorded-events recorder))
                     (sb-thread:signal-semaphore done)))))
    (unwind-protect
         (progn
           (sb-thread:wait-on-semaphore starting)
           (check :snapshot-drains-inflight
                  (not (sb-thread:wait-on-semaphore done :timeout .05))))
      (sb-thread:signal-semaphore release)
      (sb-thread:join-thread reporter)
      (sb-thread:join-thread snapshot))
    (check :snapshot-includes-inflight (= 1 (length result)))
    (funcall hook :late 2)
    (check :retired-hook-not-admitted
           (= 1 (length (lispterm.latency:recorded-events recorder))))))
(let ((recorder (lispterm.latency:make-recorder "SAFE" 4)))
  (lispterm.latency:recorded-events recorder)
  (check :retired-recorder-reuse-rejected
         (handler-case (progn (lispterm.latency:call-with-recorder recorder (constantly nil)) nil)
           (lispterm.latency:measurement-error () t))))
(multiple-value-bind (events receipt)
    (lispterm.latency:call-with-recorder
     (lispterm.latency:make-recorder "SAFE" 4)
     (lambda () (lispterm.trace:trace-event :one)))
  (check :collection-sealed-receipt
         (and (= 1 (length events)) (eq :complete (getf receipt :state))
              (equal '(:state :sealed :closed-p t :active 0 :event-count 1)
                     (getf receipt :snapshot)))))
;; Distinguish absent evidence from genuine pixel disagreement in skip controls.
(dolist (field '(:before :committed :settled))
  (let ((sample (lispterm.latency::make-sample
                 :index 0 :status :matched :before-hash 1 :buffer-hash 1 :settled-hash 2)))
    (ecase field
      (:before (setf (lispterm.latency::sample-before-hash sample) nil))
      (:committed (setf (lispterm.latency::sample-buffer-hash sample) nil))
      (:settled (setf (lispterm.latency::sample-settled-hash sample) nil)))
    (multiple-value-bind (rejected audit) (lispterm.latency:audit-agreement (vector sample))
      (check (list :missing-is-not-disagreement field)
             (and (equal '(0) rejected) (eql 1 (getf audit :matched))
                  (eql 0 (getf audit :complete))
                  (equal (list (list 0 field)) (getf audit :missing))
                  (null (getf audit :disagreements)))))))
(multiple-value-bind (rejected audit)
    (lispterm.latency:audit-agreement
     (vector (lispterm.latency::make-sample
              :index 0 :status :matched :before-hash 1 :buffer-hash 1 :settled-hash 2)))
  (check :complete-disagreement-classified
         (and (equal '(0) rejected) (eql 1 (getf audit :complete))
              (null (getf audit :missing)) (equal '(0) (getf audit :disagreements)))))
(multiple-value-bind (rejected audit)
    (lispterm.latency:audit-agreement
     (vector (lispterm.latency::make-sample
              :index 0 :status :matched :before-hash 0 :buffer-hash 1 :settled-hash 1)))
  (check :complete-agreement-including-zero-hash
         (and (null rejected) (eql 1 (getf audit :complete))
              (null (getf audit :missing)) (null (getf audit :disagreements)))))
(when (member "--benchmark" sb-ext:*posix-argv* :test #'string=)
  (load (merge-pathnames "latency-measurement-controls.lisp" *load-truename*))
  (load (merge-pathnames "latency-audit-controls.lisp" *load-truename*))
  (uiop:symbol-call :lispterm.latency.controls :recorder-overhead-controls))
(format t "~D trace safety checks; ~D failed~%" *checks* *failures*)
(sb-ext:exit :code (if (zerop *failures*) 0 1) :abort t)
