;;;; Production scheduling regression controls; device boundaries simulated.
(in-package #:lispterm.owner.tests)
(let ((session nil) (host nil) (schedule nil) (events nil) (written nil)
      (calls 0) (reads 0) (polls 0) (blocked nil) (prefix 4096) (scripted nil)
      (checks 0) (failures 0) (now 0) (read-bytes nil) (fail-key nil) (rejections nil)
      (rejection-traces nil)
      (original-event (symbol-function 'lispterm::session-event))
      (original-error (symbol-function 'lispterm::host-record-error))
      (lispterm::*input-queue-limit* 32))
  (let ((lispterm::*owner-clock* (lambda () now))
        (lispterm.trace:*trace*
          (lambda (stage time &rest fields)
            (declare (ignore time))
            (when (eq stage :input-backpressure) (push fields rejection-traces)))))
    (labels ((verify (name value)
               (incf checks) (unless value (incf failures))
               (format t "~S~%" (list :check name :pass (not (null value)))))
             (reset-case ()
               (setf session (lispterm::make-session nil 2 8)
                     host (lispterm::%make-host :session session :display :fake :state :attached)
                     schedule (lispterm::make-owner-schedule)
                     events nil written nil calls 0 reads 0 polls 0 blocked nil prefix 4096
                      now 0 read-bytes nil scripted nil fail-key nil rejections nil rejection-traces nil)
               (setf (lispterm::session-pty session) (lispterm.pty::%make-pty :open-p nil)))
             (tick () (lispterm::owner-iteration host schedule))
             (keys (text) (map 'list (lambda (c) (ltd:make-key-event :key c)) text))
             (accepted () (coerce (reverse written) 'vector)))
      (with-functions
       (list
         (cons 'lispterm::session-event
               (lambda (s e)
                 (when (and fail-key (typep e 'ltd:key-event)
                            (eql fail-key (ltd:key-event-key e)))
                   (error "Injected ordinary event failure"))
                 (funcall original-event s e)))
         (cons 'lispterm::host-record-error
               (lambda (h operation condition)
                 (when (eq operation :input-backpressure) (push condition rejections))
                 (funcall original-error h operation condition)))
        (cons 'lispterm.pty:pty-write
              (lambda (pty bytes &key timeout)
                (declare (ignore pty timeout)) (incf calls)
                (let ((n (cond (scripted (min (pop scripted) (length bytes)))
                               (blocked 0) (t (min prefix (length bytes))))))
                  (loop for i below n do (push (aref bytes i) written)) n)))
        (cons 'lispterm.pty:pty-read
              (lambda (&rest x) (declare (ignore x)) (incf reads)
                (prog1 read-bytes (setf read-bytes nil))))
        (cons 'lispterm.pty:pty-process (lambda (&rest x) (declare (ignore x)) nil))
        (cons 'lispterm::window-geometry-intent (lambda (&rest x) (declare (ignore x))))
        (cons 'ltd:poll-events (lambda (&rest x) (declare (ignore x)) (incf polls)
                               (prog1 events (setf events nil))))
        (cons 'ltd:geometry-tick (lambda (&rest x) (declare (ignore x))))
        (cons 'ltd:geometry-status (lambda (&rest x) (declare (ignore x))))
        (cons 'ltd:display-metrics (lambda (&rest x) (declare (ignore x))))
        (cons 'lispterm::render-session (lambda (&rest x) (declare (ignore x))))
        (cons 'lispterm::window-close
              (lambda (h) (setf (lispterm::host-display h) nil)
                (lispterm::set-host-state h :detached))))
       (lambda ()
         ;; A prior output read generated replies but the FIFO was full. It is
         ;; writable now. Replies must stay ahead of the newly polled key.
         (reset-case)
         (lispterm::session-write session (lispterm::utf8 (make-string 32 :initial-element #\a)))
         (setf (nerimux/terminal:screen-response-queue (lispterm::session-screen session)) '("R"))
         (setf events (keys "x"))
         (tick)
         (verify :response-backlog-keeps-healthy-display
                 (eq :attached (lispterm::host-state-value host)))
         (verify :old-input-response-key-exact-fifo
                 (equalp (accepted) (lispterm::utf8 (concatenate 'string (make-string 32 :initial-element #\a) "Rx"))))
         (verify :recoverable-backpressure-not-display-error (null (lispterm:host-errors host)))
         (verify :response-recovery-read-fairness (= reads 1))
         ;; Several events returned by one poll must not incur a sleep per event.
         (reset-case) (setf events (keys "abcdef")) (tick)
         (verify :multi-event-same-turn (equalp (accepted) (lispterm::utf8 "abcdef")))
         (verify :multi-event-one-output-read (= reads 1))
         (verify :multi-event-input-empty (zerop (lispterm::session-input-pending session)))
         ;; Positive partial writes may continue, keeping each suffix exactly once.
         (reset-case) (setf prefix 1)
         (lispterm::session-write session (lispterm::utf8 "ABCD"))
         (setf events (keys "xy")) (tick)
         (verify :partial-write-and-new-keys-same-turn
                 (equalp (accepted) (lispterm::utf8 "ABCDxy")))
         (verify :partial-write-no-duplication (= (length written) 6))
         ;; A permanently blocked FD gets bounded attempts and a positive wait.
         (reset-case)
         (lispterm::session-write session (lispterm::utf8 (make-string 32 :initial-element #\a)))
         (setf blocked t events (keys "xy"))
         (let ((wait (tick)))
           (verify :blocked-write-no-busyloop (and wait (plusp wait) (<= calls 3))))
         (verify :blocked-key-does-not-detach (eq :attached (lispterm::host-state-value host)))
         (verify :blocked-input-not-written (null written))
          (verify :blocked-events-typed-rejections
                  (and (= 2 (length rejections))
                       (every (lambda (c) (typep c 'lispterm::input-backpressure)) rejections)
                       (every (lambda (e) (eq :input-backpressure (getf e :operation)))
                              (lispterm:host-errors host))))
          (verify :blocked-events-rejection-traces (= 2 (length rejection-traces)))
          (verify :blocked-input-pending-unchanged (= 32 (lispterm::session-input-pending session)))
          (tick) (tick)
          (verify :blocked-polls-every-turn (= polls 3))
          (setf blocked nil now internal-time-units-per-second)
          (tick)
          (verify :unblock-accepted-fifo-without-rejected-input
                  (equalp (accepted) (lispterm::utf8 (make-string 32 :initial-element #\a))))
          (verify :rejections-not-retried (= 2 (length rejections)))
          ;; Quiesce does not cancel accepted bytes in the session FIFO.
          (reset-case)
          (lispterm::session-write session (lispterm::utf8 "AB"))
          (setf blocked t)
          (tick)
          (lispterm::dispatch-host-command host (lispterm::make-host-command :operation :stop))
          (verify :quiesce-preserves-accepted-bytes (= 2 (lispterm::session-input-pending session)))
          (verify :quiesced-owner-stops (null (tick)))
          (setf (lispterm::host-stop-p host) nil blocked nil
                schedule (lispterm::make-owner-schedule))
          (tick)
          (verify :accepted-input-survives-new-owner-schedule
                  (equalp (accepted) (lispterm::utf8 "AB")))
         ;; Read-time final flush can recover after both bounded drains saw EAGAIN.
         ;; Account for its positive partial write rather than sleeping on a suffix.
         (reset-case)
         (lispterm::session-write session (lispterm::utf8 "ABCD"))
         (setf scripted '(0 0 1))
         (let ((wait (tick)))
           (verify :late-write-readiness-prompt-retry (zerop wait))
           (verify :late-write-readiness-keeps-suffix (= 3 (lispterm::session-input-pending session))))
         (tick)
         (verify :late-write-readiness-no-duplicate (equalp (accepted) (lispterm::utf8 "ABCD")))
          ;; A close arriving on a later poll must not wait for PTY admission.
          (reset-case)
          (lispterm::session-write session (lispterm::utf8 (make-string 32 :initial-element #\a)))
          (setf blocked t events (keys "q")) (tick)
          (setf events (list (ltd:make-close-event))) (tick)
          (verify :later-native-close-detaches
                  (and (= polls 2) (eq :detached (lispterm::host-state-value host))))
          ;; Final close counts only accepted bytes and outstanding engine replies.
          (setf (nerimux/terminal:screen-response-queue (lispterm::session-screen session)) '("RR"))
          (let ((cancelled nil))
            (handler-case (lispterm:close host)
              (lispterm::input-cancelled (c) (setf cancelled (lispterm::input-cancelled-bytes c))))
            (verify :final-close-cancellation-exact (= (or cancelled -1) 34)))
          (verify :final-close-clears-accepted-bytes
                  (and (zerop (lispterm::session-input-pending session))
                       (null (nerimux/terminal:screen-response-queue (lispterm::session-screen session)))
                       (eq :closed (lispterm::host-state-value host))))
          (verify :second-final-close-idempotent (eq host (lispterm:close host)))
          ;; An oversized event is rejected as a whole, exactly once, then the next
          ;; event can use the empty FIFO in that same turn.
          (reset-case)
          (setf events (append (list (ltd:make-paste-event :text (make-string 33 :initial-element #\p)))
                               (keys "x")))
          (tick)
          (verify :oversized-event-rejected-once (= 1 (length rejections)))
          (verify :oversized-rejection-traced (= 1 (length rejection-traces)))
          (verify :oversized-next-key-same-turn (equalp (accepted) (lispterm::utf8 "x")))
          (verify :oversized-no-detach (eq :attached (lispterm::host-state-value host)))
          (tick)
          (verify :oversized-never-replayed
                  (and (= 1 (length rejections)) (equalp (accepted) (lispterm::utf8 "x"))))
          ;; Other failures keep baseline detach behavior. Already admitted input
          ;; survives and is written once; the unprocessed tail is not replayed.
          (reset-case) (setf fail-key #\! events (keys "a!z")) (tick) (tick)
          (verify :ordinary-error-detaches (eq :detached (lispterm::host-state-value host)))
          (verify :accepted-prefix-no-replay-after-error (equalp (accepted) (lispterm::utf8 "a")))
          (verify :ordinary-error-recorded-as-display
                  (eq :display (getf (first (lispterm:host-errors host)) :operation)))
          ;; Shared accepted FIFO order is owner processing order: commands first,
          ;; followed by the newly polled native batch. There is no native backlog.
          (reset-case)
          (setf (lispterm::host-commands host)
                (list (lispterm::make-host-command :operation :input :argument (lispterm::utf8 "C")))
                events (keys "N"))
          (tick)
          (verify :command-then-native-fifo (equalp (accepted) (lispterm::utf8 "CN")))
         ;; More than the bounded syscall budget gets a fresh owner turn, not a
         ;; sleep and not an unbounded write loop. Every turn still reads once.
         (reset-case)
         (let ((lispterm::*input-queue-limit* 256))
           (dotimes (i 80) (lispterm::session-write session (lispterm::utf8 "z")))
           (let ((wait (tick)))
             (verify :drain-budget-prompt-retry (zerop wait))
             (verify :drain-budget-bounded (<= calls 33))
             (verify :drain-budget-output-fairness (= reads 1)))
           (loop repeat 4 while (plusp (lispterm::session-input-pending session)) do (tick))
           (verify :budget-completes-all-entries-exactly
                   (equalp (accepted) (lispterm::utf8 (make-string 80 :initial-element #\z)))))
          (reset-case)
          (let ((lispterm::*input-queue-limit* 100000))
            (lispterm::session-write session (make-array 80000 :element-type '(unsigned-byte 8) :initial-element 1))
            (tick)
            (verify :drain-byte-bound (<= (length written) (* 36 1024)))
            (verify :drain-byte-bound-one-read (= reads 1)))
         ;; Existing direct caller contract still signals a typed refusal.
         (reset-case)
         (let ((typed nil))
           (handler-case (lispterm::session-write session (make-array 33 :element-type '(unsigned-byte 8) :initial-element 1))
             (lispterm::input-backpressure () (setf typed t)))
           (verify :direct-call-typed-backpressure typed))
         (format t "~S~%" (list :checks checks :failures failures))
         (when (plusp failures) (uiop:quit 1)))))))
