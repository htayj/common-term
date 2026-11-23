;;;; A fake transport exercises the real display grid, real PTY, and owner thread.
;;;; Run only in a disposable test image: these methods replace the X11 transport.
(defpackage #:lispterm.host.tests (:use #:cl))
(in-package #:lispterm.host.tests)

(defvar *checks* 0)
(defvar *fake-lock* (sb-thread:make-mutex :name "fake display transport"))
(defvar *events* nil)
(defvar *fail-poll* nil)
(defvar *fail-close* nil)
(defvar *fail-open* nil)
(defvar *close-delay* 0)
(defvar *close-started* nil)
(defvar *native-calls* nil)
(defvar *title* nil)

(defun check (name value)
  (incf *checks*)
  (format t "~S~%" (list :check name :pass (not (null value)))) (finish-output)
  (unless value (error "Host check failed: ~S" name)))
(defun raises (type function)
  (handler-case (progn (funcall function) nil) (error (condition) (typep condition type))))
(defun eventually (function &optional (timeout 5))
  (let ((end (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop when (funcall function) return t
          when (> (get-internal-real-time) end) return nil do (sleep .01))))
(defun record-native (display)
  (ltd::assert-owner display)
  (sb-thread:with-mutex (*fake-lock*)
    (push sb-thread:*current-thread* *native-calls*)))
(defmethod ltd:backend-open ((backend (eql :x11)) display)
  (declare (ignore backend)) (record-native display)
  (when *fail-open* (error 'ltd:display-error :detail "Injected native open failure")))
(defmethod ltd:backend-close ((backend (eql :x11)) display)
  (declare (ignore backend)) (record-native display)
  (when *close-started* (sb-thread:signal-semaphore *close-started*))
  (sleep *close-delay*)
  (when *fail-close* (error 'ltd:display-error :detail "Injected native close failure")))
(defmethod ltd:backend-poll ((backend (eql :x11)) display timeout)
  (declare (ignore backend timeout)) (record-native display)
  (sb-thread:with-mutex (*fake-lock*)
    (when *fail-poll*
      (setf *fail-poll* nil)
      (error 'ltd:display-connection-lost :detail "Injected transport loss"))
    ;; X11 applies the observed pixel size before publishing the resize event.
    (dolist (event *events*)
      (when (typep event 'ltd:resize-event)
        (ltd:resize-display display (ltd:resize-event-width event) (ltd:resize-event-height event))))
    (prog1 *events* (setf *events* nil))))
(defmethod ltd:backend-blit ((backend (eql :x11)) display framebuffer rectangles)
  (declare (ignore backend framebuffer rectangles)) (record-native display))
(defmethod ltd:backend-set-title ((backend (eql :x11)) display title)
  (declare (ignore backend)) (record-native display) (setf *title* title))

(defun screen-text (host)
  (let* ((session (lispterm::host-session host)) (screen (lispterm::session-screen session)))
    (sb-thread:with-mutex ((lispterm::session-lock session))
      (with-output-to-string (out)
        (dotimes (row (lispterm.engine:rows screen))
          (dotimes (col (lispterm.engine:columns screen))
            (write-char (lispterm.engine:cell-character (lispterm.engine:cell-at screen row col)) out)))))))
(defun queue-event (event)
  (sb-thread:with-mutex (*fake-lock*) (setf *events* (nconc *events* (list event)))))

(defun protocol-tests ()
  (let* ((pty (lispterm.pty:open-pty))
         (session (lispterm::make-session pty 24 80))
         (screen (lispterm::session-screen session))
         (reader (symbol-function 'lispterm.pty:pty-read))
         (writer (symbol-function 'lispterm::session-write))
         (query (lispterm::utf8 (format nil "~c[6n~c[c" #\Esc #\Esc)))
         (written nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'lispterm.pty:pty-read)
                 (lambda (&rest ignored) (declare (ignore ignored)) query)
                 (symbol-function 'lispterm::session-write)
                 (lambda (s bytes) (declare (ignore s))
                   (push (babel:octets-to-string bytes :encoding :utf-8) written)))
           (lispterm::session-read-output session)
           (check :cursor-query-reply (find (format nil "~c[1;1R" #\Esc) written :test #'equal))
           (check :device-query-reply (= 2 (length written)))
           (check :reply-queue-drained (null (nerimux/terminal:screen-response-queue screen)))
           (lispterm.engine:feed screen (lispterm::utf8 (format nil "~c[?1h~c[?2004h~c[?25l~c[?1004h" #\Esc #\Esc #\Esc #\Esc)))
           (setf written nil)
           (lispterm::session-event session (ltd:make-key-event :key :up))
           (check :application-cursor (equal (first written) (format nil "~cOA" #\Esc)))
           (lispterm::session-event session (ltd:make-paste-event :text "paste"))
           (check :bracketed-paste (equal (first written) (format nil "~c[200~~paste~c[201~~" #\Esc #\Esc)))
           (check :hidden-cursor (not (nerimux/terminal:screen-cursor-visible screen)))
           (lispterm::session-event session (ltd:make-focus-event :focused-p t))
           (check :focus-protocol (equal (first written) (format nil "~c[I" #\Esc))))
      (setf (symbol-function 'lispterm.pty:pty-read) reader
            (symbol-function 'lispterm::session-write) writer)
      (lispterm.pty:close-pty pty))))

(defun failed-open-tests ()
  (let ((pty (lispterm.pty:open-pty)) retained-host)
    (unwind-protect
         (progn
           (setf *fail-open* t)
           (check :failed-open-signalled
                  (raises 'ltd:display-open-failed (lambda () (lispterm:open :backend :x11 :pty pty))))
           (check :failed-open-preserves-supplied-pty (lispterm.pty:pty-open-p pty))
           (check :failed-open-joined-owner (null (lispterm:status)))
           (setf *fail-close* t)
           (let ((condition (handler-case (lispterm:open :backend :x11 :pty pty)
                              (lispterm:host-error (condition) condition))))
             (setf retained-host (and (typep condition 'lispterm:host-error)
                                      (lispterm:host-error-host condition)))
             (check :failed-open-retains-recovery-host retained-host)
             (check :failed-open-cleanup-pending
                    (ltd:display-cleanup-pending-p (lispterm::host-display retained-host)))
             (check :failed-open-owner-still-supervised
                    (getf (lispterm:status retained-host) :worker-alive-p))
             (check :cleanup-pending-rejects-input
                    (raises 'lispterm:host-cleanup-pending
                            (lambda () (lispterm:send-input retained-host "x"))))
             (check :cleanup-pending-rejects-reopen
                    (raises 'lispterm:host-cleanup-pending
                            (lambda () (lispterm:reopen retained-host))))
             (dotimes (attempt 2)
               (check :failed-close-signals-cleanup-pending
                      (raises 'lispterm:host-cleanup-pending
                              (lambda () (lispterm:close retained-host))))
               (check :failed-close-keeps-owner
                      (getf (lispterm:status retained-host) :worker-alive-p))
               (check :failed-close-keeps-display
                      (lispterm::host-display retained-host))
               (check :failed-close-keeps-pty (lispterm.pty:pty-open-p pty)))
             (setf *fail-close* nil)
             (lispterm:close retained-host)
             (check :cleanup-retry-joins-owner
                    (and (null (lispterm::host-thread retained-host))
                         (null (lispterm::host-display retained-host))))
             (check :cleanup-retry-closes-host
                    (eq :closed (getf (lispterm:status retained-host) :state)))
             (check :cleanup-retry-unregisters (null (lispterm:status)))
             (setf retained-host nil)))
      (setf *fail-open* nil *fail-close* nil)
      (cond (retained-host (lispterm:close retained-host))
            ((lispterm.pty:pty-open-p pty) (lispterm.pty:close-pty pty))))))

(defun run-tests ()
  (setf *checks* 0 *native-calls* nil *events* nil *fail-poll* nil *fail-close* nil)
  (protocol-tests)
  (failed-open-tests)
  (check :reject-invalid-backend
         (raises 'lispterm:host-error (lambda () (lispterm:open :backend :invalid))))
  (check :bound-grid
         (raises 'lispterm:host-error (lambda () (lispterm:open :rows 1000))))
  (check :reject-invalid-pty
         (raises 'type-error (lambda () (lispterm:open :pty :invalid))))
  (let ((pty (lispterm.pty:open-pty)))
    (lispterm.pty:close-pty pty)
    (check :reject-closed-pty
           (raises 'lispterm:host-error (lambda () (lispterm:open :pty pty)))))
  (let* ((host (lispterm:open :backend :x11 :rows 12 :columns 50
                :argv '("sh" "-c" "stty raw -echo; printf READY; cat")))
         (session (lispterm::host-session host))
         (screen (lispterm::session-screen session))
         (pty (lispterm::session-pty session))
         (pid (lispterm.pty:process-pid (lispterm.pty:pty-process pty))))
    (unwind-protect
         (progn
           (check :real-pty-output (eventually (lambda () (search "READY" (screen-text host)))))
           (check :registered (find (getf (lispterm:status host) :id) (lispterm:status) :key (lambda (s) (getf s :id))))
           (check :owner-alive (getf (lispterm:status host) :worker-alive-p))
           (check :first-frame (eventually (lambda () (plusp (getf (lispterm:status host) :frames)))))
           (let ((frames (getf (lispterm:status host) :frames)))
             (sleep .2)
             (check :unchanged-frame-skipped (<= (- (getf (lispterm:status host) :frames) frames) 1)))
           (lispterm:send-input host (format nil "~c]2;HOST-TITLE~c\\~c[2J~c[H~c[1;4;31;44mA界~c[0m" #\Esc #\Esc #\Esc #\Esc #\Esc #\Esc))
           (check :title (eventually (lambda () (equal *title* "HOST-TITLE"))))
           (check :wide-output (eventually (lambda () (search "A界" (screen-text host)))))
           (sb-thread:with-mutex ((lispterm::session-lock session))
             (let ((grid (lispterm::screen-grid screen)))
               (check :neutral-grid (typep (aref grid 0 0) 'ltd:cell))
               (check :bold (ltd:cell-bold (aref grid 0 0)))
               (check :underline (ltd:cell-underline (aref grid 0 0)))
               (check :foreground (= #xaa0000 (ltd:cell-fg (aref grid 0 0))))
               (check :background (= #x0000aa (ltd:cell-bg (aref grid 0 0))))
               (check :wide-leading (= 2 (ltd:cell-width (aref grid 0 1))))
               (check :wide-continuation (= 0 (ltd:cell-width (aref grid 0 2))))))
           ;; Inject normal close, then prove the real child and parser keep running.
           (queue-event (ltd:make-close-event))
           (check :window-close-detaches (eventually (lambda () (eq :detached (getf (lispterm:status host) :state)))))
           (lispterm:send-input host "DETACHED")
           (check :detached-output (eventually (lambda () (search "DETACHED" (screen-text host)))))
           (lispterm:reopen host)
           (check :reopen-attached (eq :attached (getf (lispterm:status host) :state)))
           (check :same-session (and (eq session (lispterm::host-session host))
                                     (eq screen (lispterm::session-screen session))
                                     (= pid (lispterm.pty:process-pid (lispterm.pty:pty-process pty)))))
           (sb-thread:with-mutex (*fake-lock*) (setf *fail-poll* t))
           (check :display-loss-detaches (eventually (lambda () (eq :detached (getf (lispterm:status host) :state)))))
           (check :observable-display-error (find :display (lispterm:host-errors host) :key (lambda (e) (getf e :operation))))
           (lispterm:send-input host "SURVIVED")
           (check :display-loss-preserves-output (eventually (lambda () (search "SURVIVED" (screen-text host)))))
           (lispterm:reopen host)
           ;; No cached function pointer: replacing the renderer affects the next frame.
           (let ((original (symbol-function 'lispterm::render-session))
                 (seen (sb-thread:make-semaphore)))
             (unwind-protect
                  (progn
                    (setf (symbol-function 'lispterm::render-session)
                          (lambda (s d) (sb-thread:signal-semaphore seen) (funcall original s d)))
                    (lispterm:invalidate host)
                    (check :live-renderer-next-frame (sb-thread:wait-on-semaphore seen :timeout 2)))
               (setf (symbol-function 'lispterm::render-session) original)))
           (queue-event (ltd:make-resize-event :rows 15 :columns 60 :width 480 :height 240))
           (check :resize-engine (eventually (lambda () (= 15 (getf (lispterm:status host) :rows)))))
           (check :resize-pty (equal '(15 60) (multiple-value-list (lispterm.pty:pty-window-size pty))))
           (setf *fail-close* t)
           (unwind-protect
                (progn
                  (check :partial-quiesce-failure-signalled
                         (raises 'ltd:display-error (lambda () (lispterm:quiesce host))))
                  (check :partial-quiesce-owner-survives (getf (lispterm:status host) :worker-alive-p))
                  (check :failed-close-handle-retryable
                         (ltd:display-cleanup-pending-p (lispterm::host-display host))))
             (setf *fail-close* nil))
           (lispterm:reopen host)
           (setf *close-delay* .2 *close-started* (sb-thread:make-semaphore))
           (unwind-protect
                (progn
                  (check :stop-timeout-signalled
                         (raises 'lispterm:host-timeout
                           (lambda () (lispterm::call-host host :stop nil .1))))
                  (check :timeout-during-native-close (sb-thread:wait-on-semaphore *close-started* :timeout 1))
                  (check :timed-out-stop-rolls-back
                         (eventually (lambda () (eq :attached (getf (lispterm:status host) :state)))))
                  (check :timed-out-stop-retains-owner (getf (lispterm:status host) :worker-alive-p)))
             (setf *close-delay* 0 *close-started* nil))
           (let ((token (lispterm:quiesce host)))
             (check :quiesce-joins (null (getf (lispterm:status host) :worker-alive-p)))
             (check :quiesce-preserves-pty (lispterm.pty:pty-open-p pty))
             (check :quiesce-state (eq :quiesced (getf (lispterm:status host) :state)))
             (lispterm:resume host token)
             (check :resume-attached (eq :attached (getf (lispterm:status host) :state)))
             (check :resume-idempotent (eq host (lispterm:resume host token)))
             (check :resume-bad-token (raises 'lispterm:host-error (lambda () (lispterm:resume host :bad)))))
           (check :all-native-calls-on-worker
                  (and *native-calls* (notany (lambda (thread) (eq thread sb-thread:*current-thread*)) *native-calls*)))
           (dotimes (i 40) (lispterm::host-record-error host :test (make-condition 'simple-error :format-control "bounded")))
           (check :bounded-error-ring (= 32 (length (lispterm:host-errors host))))
           (let ((errors (lispterm:host-errors host)))
             (setf (getf (first errors) :operation) :mutated)
             (check :error-snapshot-copy (eq :test (getf (first (lispterm:host-errors host)) :operation)))))
      (lispterm:close host))
    (check :close-idempotent (eq host (lispterm:close host)))
    (check :close-state (eq :closed (getf (lispterm:status host) :state))))
  ;; A slave may not yet be attached when an externally owned PTY is adopted.
  (let* ((pty (lispterm.pty:open-pty))
         (host (lispterm:open :backend :x11 :pty pty)))
    (unwind-protect
         (progn
           (check :adopt-without-child (null (getf (lispterm:status host) :child-pid)))
           ;; Linux reports hangup only after a slave has been opened then closed.
           (sb-posix:close (sb-posix:open (lispterm.pty:pty-slave-name pty)
                                        (logior sb-posix:o-rdwr sb-posix:o-noctty)))
           (check :eio-without-child-is-not-exit
                  (eventually (lambda () (eq :waiting-for-slave (getf (lispterm:status host) :transport-state)))))
           (check :owner-survives-eio (getf (lispterm:status host) :worker-alive-p))
           (let ((token (lispterm:quiesce host)))
             (declare (ignore token))
             ;; Model inherited held mutexes. Saver detach must never acquire
             ;; copies whose former owner vanished at fork.
             (let ((host-lock (lispterm::host-lock host))
                   (registry-lock lispterm::*hosts-lock*))
               (sb-thread:grab-mutex host-lock)
               (sb-thread:grab-mutex registry-lock)
               (unwind-protect
                    (progn
                      (lispterm:detach-for-saver host)
                      (check :saver-replaces-inherited-locks
                             (and (not (eq registry-lock lispterm::*hosts-lock*))
                                  (not (eq host-lock (lispterm::host-lock host))))))
                 (sb-thread:release-mutex registry-lock)
                 (sb-thread:release-mutex host-lock)))
             (check :saver-transport-closed (not (lispterm.pty:pty-open-p pty)))
             (check :saver-state (eq :saved (getf (lispterm:status host) :state)))))
      (lispterm:close host)))
  (check :registry-empty (null (lispterm:status)))
  (format t "(:host-checks ~D :passed T)~%" *checks*)
  t)
