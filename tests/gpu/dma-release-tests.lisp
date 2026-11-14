(in-package #:lispterm.gpu.tests)

;;; gpu-dma-release: wl_buffer release ownership and present revalidation,
;;; replayed without a compositor, a socket or a GPU. The production
;;; DMA-PRESENT, DMA-IMPORT, DMA-COMMIT, DMA-COLLECT, FLUSH-WINDOW and
;;; WIRE-DISPATCH run unchanged. Requests are recorded instead of sent, the
;;; import roundtrip delivers scripted events, and the GPU entry points are
;;; stand-ins. Sequences marked run1, run2 and run3 follow the real sway captures
;;; under .build/gpu-migration/wayland-launch-repair/b1/ (trace line numbers).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:add-package-local-nickname '#:wl '#:lispterm.display.wayland '#:lispterm.gpu.tests))

(defconstant +buffer-a+ #xff000000)
(defconstant +buffer-b+ #xff000001)
(defconstant +buffer-c+ #xff000002)
(defconstant +buffer-d+ #xff000003)
(defconstant +release-surface+ 10)

(defvar *release-sent* nil "Recorded requests (object opcode . arguments), newest first.")
(defvar *release-script* nil "Events the next import roundtrip delivers, in order.")
(defvar *release-targets* nil "Every stand-in export target, newest first.")
(defvar *release-destroyed* nil "Targets given to DMA-DESTROY, newest first.")
(defvar *release-copies* nil "Renderer copies (framebuffer width height), newest first.")

(defun release-event (state object opcode &rest words)
  "Deliver one event for OBJECT through the real dispatcher."
  (let* ((wire (wl::wayland-state-wire state))
         (body (apply #'concatenate '(vector (unsigned-byte 8)) (mapcar #'wl::u32-bytes words)))
         (size (+ 8 (length body))))
    (setf (wl::wire-bytes wire)
          (concatenate '(vector (unsigned-byte 8)) (wl::wire-bytes wire) (wl::u32-bytes object)
                       (wl::u32-bytes (logior (ash size 16) opcode)) body))
    (wl::wire-dispatch wire)))

(defun release-roundtrip (state &optional timeout)
  "Stand-in for PROTOCOL-ROUNDTRIP. A script entry is a function to call, an event
(OBJECT OPCODE . WORDS), or (:CREATED ID) answering the import in progress."
  (declare (ignore timeout))
  (let ((script (shiftf *release-script* nil))
        (params (1- (wl::wire-next-id (wl::wayland-state-wire state)))))
    (dolist (entry script)
      (cond ((functionp entry) (funcall entry))
            ((eq :created (first entry)) (release-event state params 0 (second entry)))
            (t (apply #'release-event state entry))))))

(defun release-target (context width height)
  "Stand-in for DMA-CREATE with its ownership rule: pending until a pool adopts it."
  (let ((target (lispterm.gpu::make-dma-target
                 :context context :width width :height height
                 :framebuffer (+ 100 (length *release-targets*))
                 :fourcc #x34324241 :modifier #x00ffffffffffffff)))
    (push target *release-targets*)
    (setf (gethash target lispterm.gpu::*dma-pending-targets*) t)
    target))

(defun call-with-release-doubles (function)
  "Call FUNCTION with the socket and GPU entry points replaced by recorders."
  (let* ((*release-sent* nil) (*release-script* nil) (*release-targets* nil)
         (*release-destroyed* nil) (*release-copies* nil)
         (doubles
           (list (cons 'wl::wire-send
                       (lambda (wire object opcode &rest arguments)
                         (declare (ignore wire))
                         (push (list* object opcode arguments) *release-sent*)
                         nil))
                 (cons 'wl::protocol-roundtrip #'release-roundtrip)
                 (cons 'lispterm.gpu::dma-supported-p (constantly t))
                 (cons 'lispterm.gpu::dma-create #'release-target)
                 (cons 'lispterm.gpu::dma-destroy
                       (lambda (target)
                         (push target *release-destroyed*)
                         (setf (lispterm.gpu::dma-target-closed-p target) t)
                         (remhash target lispterm.gpu::*dma-pending-targets*)
                         nil))
                 (cons 'lispterm.gpu::renderer-copy-target
                       (lambda (renderer framebuffer width height &key invert-y)
                         (declare (ignore renderer invert-y))
                         (push (list framebuffer width height) *release-copies*)
                         nil))
                 (cons 'lispterm.gpu::gl-finish (constantly nil))
                 (cons 'lispterm.gpu::gl-check (constantly nil))))
         (originals (mapcar (lambda (double) (symbol-function (car double))) doubles)))
    (unwind-protect
         (progn
           (loop for (name . double) in doubles do (setf (symbol-function name) double))
           (funcall function))
      (loop for (name) in doubles for original in originals
            do (setf (symbol-function name) original))
      (dolist (target *release-targets*)
        (remhash target lispterm.gpu::*dma-pending-targets*)))))

(defun release-world ()
  "A configured 800x480 GPU display on a socketless wire: state, dma, display, window."
  (setf *release-sent* nil *release-copies* nil)
  (let* ((state (wl::make-wayland-state :wire (wl::%make-wire :next-id 15)
                                        :surface +release-surface+ :xdg-surface 11
                                        :configured-p t :width 800 :height 480))
         (dma (wl::make-dma-protocol :id 9 :formats (list (list #x34324241 #x00ffffffffffffff))))
         (window (wl::make-wayland-window :protocol state))
         (display (lispterm.display::make-display
                   :backend :wayland :backend-state window :renderer-kind :gpu
                   :renderer-context :release-context :renderer-state :release-renderer
                   :width 800 :height 480)))
    ;; Mirror backend-open's resize sink, except for the GPU renderer resize.
    (setf (wl::wayland-state-resize-sink state)
          (lambda (width height scale)
            (setf (lispterm.display:display-width display) width
                  (lispterm.display:display-height display) height
                  (wl::wayland-window-pending-frame-p window) nil)
            (push (list :resize :width width :height height :scale scale)
                  (wl::wayland-window-events window)))
          (gethash state wl::*dma-protocols*) dma)
    (values state dma display window)))

(defun release-requests (object opcode)
  "Argument lists of the recorded requests to OBJECT with OPCODE, oldest first."
  (loop for (to code . arguments) in (reverse *release-sent*)
        when (and (eql to object) (eql code opcode)) collect arguments))

(defun release-attached ()
  "wl_buffer IDs attached to the surface, oldest first."
  (mapcar #'first (release-requests +release-surface+ 1)))

(defun release-buffer (dma id)
  (find id (wl::dma-protocol-buffers dma) :key #'wl::dma-buffer-id))

(defun release-counts (state)
  "(releases late-releases) as DMA-METRICS publishes them."
  (let ((metrics (wl::dma-metrics state)))
    (list (getf metrics :releases) (getf metrics :late-releases))))

(defun release-intact-p (dma buffer busy)
  "BUFFER is pooled exactly once with a live wl_buffer, an open target and busy state BUSY."
  (and (= 1 (count buffer (wl::dma-protocol-buffers dma)))
       (eq busy (wl::dma-buffer-busy-p buffer))
       (not (wl::dma-buffer-protocol-destroyed-p buffer))
       (not (member (wl::dma-buffer-target buffer) *release-destroyed*))
       (not (lispterm.gpu::dma-target-closed-p (wl::dma-buffer-target buffer)))))

(defun release-present (display &rest script)
  "Run the real DMA-PRESENT; SCRIPT is delivered inside its import roundtrip."
  (setf *release-script* script)
  (prog1 (wl::dma-present display)
    (check (null *release-script*) "Present made no import roundtrip for its scripted events")))

(defun release-must-present (display &rest script)
  (let ((result (apply #'release-present display script)))
    (check (eq :presented result) "Present returned ~S, expected :PRESENTED" result)))

(defun release-frame-done (state)
  "Deliver the frame callback of the latest commit."
  (release-event state (first (first (last (release-requests +release-surface+ 3)))) 0 0))

(defun release-rejects (description operation thunk)
  "THUNK must signal the transport error whose operation is OPERATION."
  (let ((condition (handler-case (progn (funcall thunk) nil)
                     (wl::wayland-transport-error (condition) condition))))
    (check condition "~A was accepted" description)
    (check (equal operation (wl::transport-operation condition))
           "~A raised ~S, expected ~S" description (wl::transport-operation condition) operation)))

(defun release-two-commits (state display)
  "run3 lines 35-83: A committed at 800x480, configure 1710x453, B committed, A released once."
  (release-must-present display `(:created ,+buffer-a+))
  (release-frame-done state)
  (wl::protocol-size state 1710 453)
  (release-must-present display `(:created ,+buffer-b+))
  (release-event state +buffer-a+ 0)
  (release-frame-done state))

;;; Cases

(defun release-transition-table ()
  (loop for (busy retired opcode length expected)
          in '((t nil 0 0 :release)
               (nil nil 0 0 "duplicate dma release")
               (nil t 0 0 :late-release)
               (t t 0 0 :late-release)
               (t nil 1 0 "invalid dma wl_buffer.release")
               (t nil 0 4 "invalid dma wl_buffer.release")
               (nil nil 1 0 "invalid dma wl_buffer.release")
               (nil t 1 0 "invalid dma wl_buffer.release")
               (nil t 0 4 "invalid dma wl_buffer.release"))
        for actual = (wl::dma-release-transition busy retired opcode length)
        do (check (equal expected actual) "busy ~S retired ~S opcode ~S length ~S gave ~S, expected ~S"
                  busy retired opcode length actual expected))
  t)

(defun release-run3-replay ()
  "run3 lines 35-92: the release for destroyed A arrives inside the third import roundtrip."
  (multiple-value-bind (state dma display) (release-world)
    (release-two-commits state display)
    (let ((b (release-buffer dma +buffer-b+))
          (a-target (wl::dma-buffer-target (release-buffer dma +buffer-a+))))
      (release-must-present display (list +buffer-a+ 0) `(:created ,+buffer-c+))
      (check (equal '(1 1) (release-counts state))
             "(releases late-releases) is ~S, expected (1 1)" (release-counts state))
      (check (= 1 (length (release-requests +buffer-a+ 0))) "A was not destroyed exactly once")
      (check (= 1 (count a-target *release-destroyed*)) "A's target was not freed exactly once")
      (check (release-intact-p dma b t) "Busy B was disturbed by A's late release")
      (check (release-intact-p dma (release-buffer dma +buffer-c+) t) "C was not committed")
      (check (equal (list +buffer-a+ +buffer-b+ +buffer-c+) (release-attached))
             "Attached ~S" (release-attached))))
  t)

(defun release-live-idle-duplicate ()
  "run2 prefix: without a third present A stays live and idle, so a second release is an error."
  (multiple-value-bind (state dma display) (release-world)
    (release-two-commits state display)
    (let ((a (release-buffer dma +buffer-a+)))
      (release-rejects "Second release on live idle A" "duplicate dma release"
                       (lambda () (release-event state +buffer-a+ 0)))
      (check (release-intact-p dma a nil) "Live idle A changed state")
      (check (equal '(1 0) (release-counts state))
             "(releases late-releases) is ~S, expected (1 0)" (release-counts state))))
  t)

(defun release-never-committed ()
  (multiple-value-bind (state dma) (release-world)
    (setf *release-script* `((:created ,+buffer-a+)))
    (let ((a (wl::dma-import state dma (release-target :release-context 800 480))))
      (check (and a (eql +buffer-a+ (wl::dma-buffer-id a)) (zerop (wl::dma-buffer-uses a)))
             "Import did not bind A")
      (release-rejects "Release on never-committed A" "duplicate dma release"
                       (lambda () (release-event state +buffer-a+ 0)))
      (check (release-intact-p dma a nil) "Never-committed A changed state")
      (check (equal '(0 0) (release-counts state))
             "(releases late-releases) is ~S, expected (0 0)" (release-counts state))))
  t)

(defun release-malformed ()
  (flet ((rejects (state what &rest event)
           (release-rejects what "invalid dma wl_buffer.release"
                            (lambda () (apply #'release-event state +buffer-a+ event)))))
    (multiple-value-bind (state dma display) (release-world)
      (release-must-present display `(:created ,+buffer-a+))
      (rejects state "Opcode 1 on busy A" 1)
      (rejects state "Release with payload on busy A" 0 7)
      (check (release-intact-p dma (release-buffer dma +buffer-a+) t) "Busy A changed state")
      (check (equal '(0 0) (release-counts state))
             "(releases late-releases) is ~S, expected (0 0)" (release-counts state)))
    (multiple-value-bind (state dma display) (release-world)
      (release-two-commits state display)
      (release-must-present display `(:created ,+buffer-c+))
      (rejects state "Opcode 1 on destroyed A" 1)
      (rejects state "Release with payload on destroyed A" 0 7)
      (check (equal '(1 0) (release-counts state))
             "(releases late-releases) is ~S, expected (1 0)" (release-counts state))
      (check (and (release-intact-p dma (release-buffer dma +buffer-b+) t)
                  (release-intact-p dma (release-buffer dma +buffer-c+) t))
             "A malformed event on destroyed A disturbed B or C")))
  t)

(defun release-id-reuse ()
  "The compositor gives destroyed A's ID to the third buffer."
  (multiple-value-bind (state dma display) (release-world)
    (release-two-commits state display)
    (let* ((handlers (wl::wire-handlers (wl::wayland-state-wire state)))
           (old (gethash +buffer-a+ handlers))
           (b (release-buffer dma +buffer-b+)))
      (release-must-present display (list +buffer-a+ 0) `(:created ,+buffer-a+))
      (let ((c (release-buffer dma +buffer-a+)))
        (check (and c (not (eq c b)) (release-intact-p dma c t)) "C did not take over A's ID")
        (check (not (eq old (gethash +buffer-a+ handlers))) "A's handler still owns the reused ID")
        (check (not (member +buffer-a+ (wl::dma-protocol-retired dma))) "Reused ID is still retired")
        (check (= 1 (length (release-requests +buffer-a+ 0))) "The reused ID was destroyed again")
        ;; A late call of A's own callback must not release or free C.
        (funcall old +buffer-a+ 0 (vector))
        (check (equal '(1 2) (release-counts state))
               "(releases late-releases) is ~S, expected (1 2)" (release-counts state))
        (check (and (release-intact-p dma c t) (release-intact-p dma b t))
               "A's late callback disturbed C or B")
        (release-rejects "Opcode 1 on reused ID" "invalid dma wl_buffer.release"
                         (lambda () (release-event state +buffer-a+ 1)))
        (check (release-intact-p dma c t) "A malformed event released C")
        (release-event state +buffer-a+ 0)
        (check (release-intact-p dma c nil) "C's own release did not make it idle")
        (release-rejects "Second release on live idle C" "duplicate dma release"
                         (lambda () (release-event state +buffer-a+ 0)))
        (check (equal '(2 2) (release-counts state))
               "(releases late-releases) is ~S, expected (2 2)" (release-counts state))
        (check (release-intact-p dma b t) "B changed state"))))
  t)

(defun release-created-wrong-target ()
  (multiple-value-bind (state dma display) (release-world)
    (release-two-commits state display)
    (let* ((handlers (wl::wire-handlers (wl::wayland-state-wire state)))
           (b (release-buffer dma +buffer-b+)) (handler (gethash +buffer-b+ handlers)))
      (release-rejects "created reusing live B's ID" "dma-buf created reuses a live wl_buffer ID"
                       (lambda () (release-present display `(:created ,+buffer-b+))))
      (check (and (release-intact-p dma b t) (eq handler (gethash +buffer-b+ handlers)))
             "Live B lost its ID or handler")
      (check (equal (list +buffer-a+ +buffer-b+) (release-attached)) "Attached ~S" (release-attached))))
  (multiple-value-bind (state dma display) (release-world)
    (declare (ignore dma))
    (let ((handlers (wl::wire-handlers (wl::wayland-state-wire state)))
          (surface (lambda (id opcode payload) (declare (ignore id opcode payload)))))
      (setf (gethash +release-surface+ handlers) surface)
      (release-rejects "created with the surface's client ID" "dma-buf created ID outside the server range"
                       (lambda () (release-present display `(:created ,+release-surface+))))
      (check (eq surface (gethash +release-surface+ handlers)) "The surface handler was replaced")
      (check (null (release-attached)) "Attached ~S" (release-attached))))
  t)

(defun release-disconnect ()
  (multiple-value-bind (state dma display) (release-world)
    (release-two-commits state display)
    (release-must-present display `(:created ,+buffer-c+))
    (check (equal (list +buffer-a+) (wl::dma-protocol-retired dma)) "Retired IDs ~S"
           (wl::dma-protocol-retired dma))
    (let ((requests (length *release-sent*)))
      (wl::dma-close-display display)
      (check (= requests (length *release-sent*)) "Requests were sent after disconnect")
      (check (loop for id being the hash-keys of (wl::wire-handlers (wl::wayland-state-wire state))
                   never (>= id #xff000000))
             "A wl_buffer handler outlived disconnect")
      (check (and (= 3 (length *release-targets*))
                  (every (lambda (target) (= 1 (count target *release-destroyed*))) *release-targets*))
             "Targets were not each freed exactly once")
      (check (null (gethash state wl::*dma-protocols*)) "The protocol record outlived disconnect")))
  t)

(defun release-configure-case (late)
  "run1 lines 257-280: a configure changes generation and size inside the third import
roundtrip. LATE adds the release for destroyed A that run1 also received there."
  (multiple-value-bind (state dma display window) (release-world)
    (release-two-commits state display)
    (wl::protocol-size state 1710 465)
    (let* ((result nil)
           (script `(,(lambda ()
                         (wl::protocol-size state 1710 453)
                         (check (not (wl::wayland-window-pending-frame-p window))
                                "The reentrant resize sink did not clear the pending frame"))
                     ,@(when late (list (list +buffer-a+ 0)))
                     (:created ,+buffer-c+)))
           (wl::*direct-present-hook*
             (lambda (display) (setf result (apply #'release-present display script)))))
      (setf (wl::wayland-window-pending-frame-p window) t)
      (wl::flush-window display window)
      (let* ((c (release-buffer dma +buffer-c+)) (target (and c (wl::dma-buffer-target c))))
        (check (eq :busy result) "Present over a configure returned ~S, expected :BUSY" result)
        (check (equal '(3 1710 453 1710 453)
                      (list (wl::wayland-state-generation state) (wl::wayland-state-width state)
                            (wl::wayland-state-height state) (lispterm.display:display-width display)
                            (lispterm.display:display-height display)))
               "The configure did not change state and display")
        (check (equal (list +buffer-a+ +buffer-b+) (release-attached))
               "Stale present attached ~S" (release-attached))
        (check (= 2 (length (release-requests +release-surface+ 6))) "Stale present committed")
        (check (and (= 2 (length *release-copies*))
                    (not (find (lispterm.gpu::dma-target-framebuffer target) *release-copies* :key #'first)))
               "Stale present copied: ~S" *release-copies*)
        (check (and (release-intact-p dma c nil) (wl::dma-buffer-complete-p c)
                    (zerop (wl::dma-buffer-uses c)) (= 2 (wl::dma-buffer-generation c))
                    (equal '(1710 465) (list (lispterm.gpu::dma-target-width target)
                                             (lispterm.gpu::dma-target-height target)))
                    (not (gethash target lispterm.gpu::*dma-pending-targets*)))
               "The pool does not own imported C as an idle stale-generation buffer")
        (check (and (not (wl::wayland-state-frame-pending-p state))
                    (wl::wayland-window-pending-frame-p window))
               "The frame is not left pending for the next attempt")
        (check (= 2 (getf (wl::dma-metrics state) :presents)) "Presents ~S" (wl::dma-metrics state))
        (check (equal (list 1 (if late 1 0)) (release-counts state))
               "(releases late-releases) is ~S" (release-counts state))
        ;; The next attempt collects C and commits the configured size.
        (setf script `((:created ,+buffer-d+)))
        (wl::flush-window display window)
        (let ((d (release-buffer dma +buffer-d+)))
          (check (eq :presented result) "Present after the configure returned ~S" result)
          (check (and (= 1 (count target *release-destroyed*))
                      (= 1 (length (release-requests +buffer-c+ 0)))
                      (not (member c (wl::dma-protocol-buffers dma))))
                 "Stale C was not destroyed exactly once")
          (check (and d (release-intact-p dma d t)
                      (equal (list +buffer-a+ +buffer-b+ +buffer-d+) (release-attached))
                      (equal '(0 0 1710 453) (first (last (release-requests +release-surface+ 2))))
                      (equal (list (lispterm.gpu::dma-target-framebuffer (wl::dma-buffer-target d)) 1710 453)
                             (first *release-copies*)))
                 "The configured size was not copied and committed from D")
          (check (not (wl::wayland-window-pending-frame-p window)) "The frame stayed pending")))))
  t)

(defun release-configure-during-import () (release-configure-case nil))
(defun release-run1-configure-and-late-release () (release-configure-case t))

(defparameter *release-cases*
  '(release-transition-table release-run3-replay release-live-idle-duplicate
    release-never-committed release-malformed release-id-reuse release-created-wrong-target
    release-disconnect release-configure-during-import release-run1-configure-and-late-release))

(defun gpu-dma-release-check ()
  (let ((failed nil))
    (dolist (name *release-cases*)
      (handler-case
          (progn
            (check (eq t (call-with-release-doubles name)) "Case did not establish success")
            (format t "DMA-RELEASE ~(~A~) PASS~%" name))
        (error (condition)
          (push name failed)
          (format t "DMA-RELEASE ~(~A~) FAIL ~A~%" name condition))))
    (check (null failed) "~D of ~D release cases failed: ~(~{~A~^ ~}~)"
           (length failed) (length *release-cases*) (reverse failed)))
  t)

(register-check "gpu-dma-release" #'gpu-dma-release-check)
