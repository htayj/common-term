;;;; Deterministic metadata/protocol tests. No display, PTY or PNG.
(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system "lispterm")
(load (merge-pathnames "vttest-protocol.lisp" *load-truename*))
(let ((output (merge-pathnames "on-phase-method.fasl"
                 (uiop:ensure-directory-pathname
                   (or (uiop:getenv "LISPTERM_HARNESS_TEST_ARTIFACTS")
                       (error "Fresh test artifact directory required"))))))
  (multiple-value-bind (path warnings failure)
      (compile-file (merge-pathnames "vttest-on-phase.lisp" *load-truename*) :output-file output)
    (declare (ignore warnings)) (assert (not failure)) (load path)))
(in-package #:vttest-on-phase)
(defparameter *checks* 0)
(defun check (value &optional description)
  (incf *checks*) (unless value (error "Check failed: ~a" description)))
(defun sample (phase frame time &optional (backend :x11))
  (let* ((token (list :sequence frame :frame frame :session 1 :phase phase :time (- time 1/10)
                      :pty (list :master-fd 12 :slave "/dev/pts/fake")
                      :width 640 :height 384 :buffer-scale 1
                      :output 99 :bytes 99 :window 7 :surface 8 :outputs '(9)))
         (status (list :id 1 :backend backend :child-pid 21 :rows 24 :columns 80
                       :pty-rows 24 :pty-columns 80 :engine-geometry-generation 2
                       :bytes-received 99 :frames frame :blink-phase phase :blink-visible t
                       :input-pending 0 :process-state :running
                       :geometry (copy-tree '(:phase :settled :request-id 2 :display-epoch 3 :epoch 2
                                   :actual-width 640 :actual-height 384 :buffer-scale 1)))))
    (list :time time :uptime (+ 100 time) :session 1 :pty (copy-tree (getf token :pty))
          :status status :output 99 :submitted token
          :ack (list :time (- time 1/20) :token (copy-tree token)
                     :kind (if (eq backend :x11) :x11-ordered-get-geometry :wp-presentation-presented)
                     :clock-id 1 :outputs '(9) :width 640 :height 384))))
(defun packet (&optional (backend :x11))
  (let ((pre (sample t 3 2 backend)) (post (sample t 3 3 backend)))
    (setf (getf post :submitted) (copy-tree (getf pre :submitted))
          (getf (getf post :ack) :token) (copy-tree (getf pre :submitted)))
    (list :error nil :off (sample nil 2 1 backend) :pre pre :post post
          :request (list :time 21/10 :uptime 1021/10) :ack (list :time 29/10 :uptime 1029/10)
          :external (list :start 1022/10 :end 1028/10 :exit 0 :png t :identity-stable t
                          :client (sb-posix:getpid) :window 7 :output "private-output"
                          :width 640 :height 384 :scale 1 :pixel-width 640 :pixel-height 384))))
(defun rejected (change &optional reason)
  (let ((p (packet)))
    (funcall change p)
    (multiple-value-bind (ok why) (validate-attempt p)
      (check (not ok) (list :rejection why))
      (when reason (check (eq reason why) (list :reason reason why))))))
(setf *scale* 1)
(dolist (backend '(:x11 :wayland)) (check (validate-attempt (packet backend)) backend))
(let ((found nil))
  (dolist (backend '(:x11 :wayland :headless))
    (dolist (label '("screen" "bug-d" "vt52" "keyboard-cursor"))
      (dolist (suffix '("pause13" "pause14" "pause15" "choice0" "keys0-up"))
        (when (eligible-p backend label suffix) (push (list backend label suffix) found)))))
  (check (equal (reverse found) '((:x11 "screen" "pause14") (:x11 "bug-d" "choice0")
                                 (:wayland "screen" "pause14"))) :exact-dispatch))
(rejected (lambda (p) (setf (getf p :off) nil)) :partial-packet)
(rejected (lambda (p) (setf (getf (getf p :off) :status) (getf (getf p :pre) :status)))
          :missing-phase-specific-presentation)
(rejected (lambda (p) (setf (getf (getf (getf p :pre) :ack) :kind) :output-only))
          :missing-phase-specific-presentation)
(rejected (lambda (p) (setf (getf (getf (getf p :pre) :ack) :token)
                            (getf (getf p :off) :submitted))) :missing-phase-specific-presentation)
(rejected (lambda (p) (incf (getf (getf (getf p :pre) :status) :frames)))
          :missing-phase-specific-presentation)
(rejected (lambda (p) (incf (getf (getf p :post) :output))) :target-output-or-geometry-changed)
(rejected (lambda (p) (incf (getf (getf (getf p :post) :status) :bytes-received)))
          :target-output-or-geometry-changed)
(rejected (lambda (p) (incf (getf (getf (getf (getf p :post) :status) :geometry) :epoch)))
          :target-output-or-geometry-changed)
(rejected (lambda (p) (incf (getf (getf (getf p :post) :status) :child-pid)))
          :target-output-or-geometry-changed)
(rejected (lambda (p) (setf (getf (getf (getf p :post) :status) :blink-phase) nil))
          :missing-phase-specific-presentation)
(rejected (lambda (p)
            (let ((post (getf p :post)))
              (incf (getf (getf post :status) :frames))
              (incf (getf (getf post :submitted) :frame))
              (incf (getf (getf post :submitted) :sequence))
              (setf (getf (getf post :ack) :token) (copy-tree (getf post :submitted)))))
          :frame-changed-during-exposure)
(dolist (change (list
                 (lambda (p) (setf (getf (getf p :request) :time) 1))
                 (lambda (p) (setf (getf (getf p :external) :start) 100))
                 (lambda (p) (setf (getf (getf p :external) :end) 104))))
  (rejected change :acquisition-interval-invalid))
;; Isolate each ten-second limit with otherwise ordered acquisition brackets.
(rejected (lambda (p)
            (setf (getf (getf p :external) :end) 1132/10
                  (getf (getf p :ack) :uptime) 1133/10))
          :acquisition-interval-invalid)
(rejected (lambda (p)
            (setf (getf (getf p :ack) :time) 13
                  (getf (getf p :post) :time) 14))
          :acquisition-interval-invalid)
(dolist (change (list
                 (lambda (p) (setf (getf (getf p :external) :exit) 124))
                 (lambda (p) (setf (getf (getf p :external) :png) nil))
                 (lambda (p) (setf (getf (getf p :external) :identity-stable) nil))
                 (lambda (p) (incf (getf (getf p :external) :client)))
                 (lambda (p) (incf (getf (getf p :external) :width)))
                 (lambda (p) (incf (getf (getf p :external) :scale)))
                 (lambda (p) (incf (getf (getf p :external) :window)))))
  (rejected change :external-identity-or-image-invalid))
(rejected (lambda (p) (setf (getf p :error) :screenshot-timeout)) :acquisition-error)
(check (not (validate-attempt '(:pre :malformed))) :malformed)
(let ((p (packet :wayland)))
  (setf (getf (getf (getf p :pre) :ack) :kind) :wl-surface-frame)
  (check (not (validate-attempt p)) :frame-callback-is-not-presentation))
(let ((p (packet :wayland)))
  (setf (getf (getf (getf p :pre) :ack) :outputs) '(99))
  (check (not (validate-attempt p)) :wrong-presentation-output))
(let ((p (packet)))
  (setf (getf (getf (getf p :off) :status) :geometry)
        (append (getf (getf (getf p :off) :status) :geometry) '(:frame-generation 2))
        (getf (getf (getf p :pre) :status) :geometry)
        (append (getf (getf (getf p :pre) :status) :geometry) '(:frame-generation 3)))
  (check (validate-attempt p) :frame-counter-not-geometry))

(defmacro with-functions (bindings &body body)
  (let ((saved (gensym)))
    `(let ((,saved (mapcar (lambda (symbol) (cons symbol (symbol-function symbol)))
                          ',(mapcar #'first bindings))))
       (unwind-protect
            (progn ,@(mapcar (lambda (binding) `(setf (symbol-function ',(first binding)) ,(second binding))) bindings)
                   ,@body)
         (dolist (entry ,saved) (setf (symbol-function (car entry)) (cdr entry)))))))

;; Real test-side wire wrappers, fake transport only. Feedback must precede its
;; commit; neither submission nor byte count alone establishes presentation.
(let* ((root (uiop:ensure-directory-pathname (uiop:getenv "LISPTERM_HARNESS_TEST_ARTIFACTS")))
       (*root* root) (*write* #'vttest-check:write-value)
       (wire (lispterm.display.wayland::%make-wire))
       (state (lispterm.display.wayland::make-wayland-state :wire wire :surface 8 :entered-outputs '(9)))
       (window (lispterm.display.wayland::make-wayland-window :protocol state))
       (display (lispterm.display::make-display :backend :wayland :backend-state window :blink-phase nil))
       (session (lispterm::%make-session :bytes-received 99)) (requests nil) (feedback nil)
       (symbols '(lispterm::render-session lispterm.display:present
                  lispterm.display.wayland::flush-window lispterm.display.wayland::protocol-present
                  lispterm.display.wayland::bind-global lispterm.display.wayland::wire-send)))
  (let ((saved (mapcar (lambda (s) (cons s (symbol-function s))) symbols)))
    (unwind-protect
         (with-functions
             ((lispterm::render-session
                (lambda (s d &rest keys) (declare (ignore s keys)) (lispterm.display:present d)))
              (lispterm.display:present
                (lambda (d) (lispterm.display.wayland::flush-window d window)))
              (lispterm.display.wayland::flush-window
                (lambda (d w) (declare (ignore d w))
                  (lispterm.display.wayland::protocol-present state #() 640 384)))
              (lispterm.display.wayland::protocol-present
                (lambda (s pixels width height) (declare (ignore s pixels width height))
                  (lispterm.display.wayland::wire-send wire 8 6) t))
              (lispterm.display.wayland::bind-global
                (lambda (&rest args) (declare (ignore args)) :ordinary-global))
              (lispterm.display.wayland::registry-bind
                (lambda (s name interface version handler)
                  (declare (ignore s name interface version))
                  (funcall handler 100 0 (lispterm.display.wayland::u32-bytes 1)) 100))
              (lispterm.display.wayland::wire-send
                (lambda (w object opcode &rest args) (declare (ignore w))
                  (push (list object opcode args) requests)
                  (when (and (= object 100) (= opcode 1)) (setf feedback (second args))))))
           (install root #'vttest-check:write-value (lambda () 99) (lambda (h) (declare (ignore h))) 1)
           (check (eq :ordinary-global (lispterm.display.wayland::bind-global state 1 "other" 1)))
           (lispterm.display.wayland::bind-global state 1 "wp_presentation" 2)
           (lispterm::render-session session display)
           (check (equal (reverse requests) (list (list 100 1 (list 8 feedback)) '(8 6 nil))) :feedback-before-commit)
           (let ((token (gethash display *submitted*)))
             (check (and token (null (getf token :phase)) (= 1 (getf token :frame))) :submitted-phase)
             (check (null (gethash display *acknowledged*)) :commit-not-presentation)
             (let ((handler (gethash feedback (lispterm.display.wayland::wire-handlers wire))))
               (funcall handler feedback 0 (lispterm.display.wayland::u32-bytes 9))
               (funcall handler feedback 1 (make-array 28 :element-type '(unsigned-byte 8) :initial-element 0)))
             (check (equal token (getf (gethash display *acknowledged*) :token)) :feedback-token)
             (check (eq :wp-presentation-presented (getf (gethash display *acknowledged*) :kind)))
             (check (null (gethash feedback (lispterm.display.wayland::wire-handlers wire))) :destructor)))
      (dolist (entry saved) (setf (symbol-function (car entry)) (cdr entry))))))

;; Failed acquisition retains the packet/inventory and stops without a PNG.
(let* ((*root* (uiop:ensure-directory-pathname (uiop:getenv "LISPTERM_VTTEST_ARTIFACTS")))
       (*write* #'vttest-check:write-value) (*attempts* nil))
  (with-functions
      ((lispterm:status (lambda (host) (declare (ignore host)) '(:backend :x11)))
       (wait-transition (lambda (host end) (declare (ignore host end))
                          (error "Simulated acquisition observable missing"))))
    (check (null (capture :fake "screen" "pause14")))
    (check (= 1 (length *attempts*)) :stop-on-unexpected-failure)
    (check (getf (first *attempts*) :error))
    (check (probe-file (merge-pathnames "x11-screen-pause14-attempt01.attempt.sexp" *root*)))
    (check (probe-file (merge-pathnames "inventory.sexp" *root*)))
    (check (null (directory (merge-pathnames "*.png" *root*))))))
;; Exercise the real capture loop and validator with synthetic samples and fake
;; external delivery. Clocks advance normally; this establishes no native timing.
(defun check-rejected-capture (name change reason expected-count &optional unknown-reason)
  (let* ((*root* (merge-pathnames (format nil "~a/" name)
                   (uiop:ensure-directory-pathname (uiop:getenv "LISPTERM_VTTEST_ARTIFACTS"))))
         (*attempts* nil) (count 0) (fixture nil)
         (validator (symbol-function 'validate-attempt))
         (*write*
           (lambda (path value)
             (vttest-check:write-value path value)
             (when (search ".pending.sexp" (namestring path))
               ;; Fake delivery after the real request timestamp, before its ACK.
               (setf (getf (getf fixture :external) :start) (uptime)
                     (getf (getf fixture :external) :end) (uptime))
               (funcall change fixture)
               (let ((prefix (getf value :name)))
                 (vttest-check:write-value
                   (merge-pathnames (concatenate 'string prefix ".external.sexp") *root*)
                   (getf fixture :external))
                 (with-open-file (out (merge-pathnames (concatenate 'string prefix ".ack") *root*)
                                      :direction :output :if-exists :error)
                   (write-line "fake acquisition complete" out)))))))
    (ensure-directories-exist (merge-pathnames "placeholder" *root*))
    (with-functions
        ((lispterm:status (lambda (host) (declare (ignore host)) '(:backend :x11)))
         (wait-transition
           (lambda (host end)
             (declare (ignore host end))
             (when (plusp count)
               (rename-file (merge-pathnames "capture-request" *root*)
                            (merge-pathnames (format nil "request~d.txt" count) *root*)))
             (incf count)
             (let ((now (monotonic)))
               (setf fixture (packet)
                     (getf fixture :off) (sample nil 2 (- now 1/5))
                     (getf fixture :pre) (sample t 3 now)
                     (getf fixture :post) (copy-tree (getf fixture :pre))))
             (values (getf fixture :off) (getf fixture :pre))))
         (snapshot (lambda (host)
                     (declare (ignore host))
                     (setf (getf (getf fixture :post) :time) (monotonic))
                     (getf fixture :post)))
         (validate-attempt
           (lambda (p)
             (multiple-value-bind (ok why) (funcall validator p)
               ;; Only the unknown-reason forward-compatibility case overrides
               ;; the real validator's rejection; all other cases use it intact.
               (values ok (if (and unknown-reason (not ok)) :unknown-rejection why))))))
      (check (null (capture :fake "screen" "pause14")) name))
    (check (= expected-count count (length *attempts*)) (list name :attempt-count))
    (check (every (lambda (a) (and (not (getf a :selected)) (eq reason (getf a :reason))))
                  *attempts*) (list name :reasons))
    (dolist (attempt *attempts*)
      (dolist (suffix '(".pending.sexp" ".external.sexp" ".ack" ".attempt.sexp"))
        (check (probe-file (merge-pathnames
                            (concatenate 'string (getf attempt :name) suffix) *root*))
               (list name suffix)))
      (let ((retained (vttest-check:read-value
                        (merge-pathnames (concatenate 'string (getf attempt :name) ".attempt.sexp")
                                         *root*))))
        (check (and (eq reason (getf retained :reason))
                    (not (getf retained :selected))
                    (= (getf attempt :attempt) (getf retained :attempt)))
               (list name :retained-rejection))))
    (check (= expected-count (length (getf (vttest-check:read-value
                                  (merge-pathnames "inventory.sexp" *root*)) :attempts))))
    (check (eq :unverified (getf (vttest-check:read-value
                                  (merge-pathnames "x11-screen-pause14.result.sexp" *root*))
                                 :disposition)))
    (check (probe-file (merge-pathnames "capture-request" *root*)))
    (check (null (directory (merge-pathnames "*.png" *root*))))
    (format t "~&Capture fixture ~a: ~d retained, ~s, UNVERIFIED.~%" name count reason)))

(defun race-frame (p)
  (let ((post (getf p :post)))
    (incf (getf (getf post :status) :frames))
    (incf (getf (getf post :submitted) :frame))
    (incf (getf (getf post :submitted) :sequence))
    (setf (getf (getf post :ack) :token) (copy-tree (getf post :submitted)))))

(check-rejected-capture "frame-race" #'race-frame :frame-changed-during-exposure 6)
(dolist (mixed '(nil t))
  (dolist (failure '(:malformed-packet :acquisition-interval-invalid :overlong :identity))
    (check-rejected-capture
      (format nil "~(~a~)-~a" failure (if mixed "with-frame-race" "alone"))
      (lambda (p)
        (when mixed (race-frame p))
        (ecase failure
          (:malformed-packet (setf (getf (getf p :external) :start) nil))
          (:acquisition-interval-invalid (setf (getf (getf p :external) :start) 0))
          (:overlong (incf (getf (getf p :external) :end) 11))
          (:identity (setf (getf (getf p :external) :identity-stable) nil))))
      (case failure (:overlong :acquisition-interval-invalid)
                    (:identity :external-identity-or-image-invalid) (t failure))
      1)))
(check-rejected-capture "missing-presentation"
  (lambda (p) (setf (getf (getf (getf p :post) :ack) :kind) :output-only))
  :missing-phase-specific-presentation 1)
(check-rejected-capture "unknown-rejection" #'race-frame :unknown-rejection 1 t)
;; Initial ON is insufficient; sample the real method's transition automaton.
(let* ((*root* (uiop:ensure-directory-pathname (uiop:getenv "LISPTERM_HARNESS_TEST_ARTIFACTS")))
       (*check* (lambda (host) (declare (ignore host))))
       (samples (list (sample t 1 0) (sample nil 2 1) (sample t 3 2))) (reads 0))
  (with-functions ((snapshot (lambda (host) (declare (ignore host)) (incf reads) (pop samples))))
    (multiple-value-bind (off on) (wait-transition :fake (+ (monotonic) 1))
      (check (and off on (= 3 reads)) :initial-on-ignored-until-off-on)))
  (with-functions ((snapshot (lambda (host) (declare (ignore host)) (sample t 1 0))))
    (multiple-value-bind (off on last) (wait-transition :fake (monotonic))
      (check (and (null off) (null on) last) :transition-deadline))))

;; Execute the actual shell helper with fake ownership/tools. Failed external
;; capture must retain the error log, metadata, original request and ACK, no PNG.
(let* ((dir (merge-pathnames "shell-fixture/"
              (uiop:ensure-directory-pathname (uiop:getenv "LISPTERM_HARNESS_TEST_ARTIFACTS"))))
       (source (uiop:read-file-string "tests/vttest-gate.sh"))
       (start (search "supplement_capture() {" source))
       (end (search (format nil "~%}~%") source :start2 start))
       (script (merge-pathnames "exercise.sh" dir)))
  (ensure-directories-exist (merge-pathnames "on-phase/placeholder" dir))
  (with-open-file (out script :direction :output :if-exists :error)
    (format out "set -Eeuo pipefail~%art=~s~%" (namestring dir))
    (write-string "backend=x11; scale=1; client=$BASHPID; DISPLAY=:FAKE
fixture_isolation() { echo mock-owned-endpoint; }
window_identity() { window=7; }
timeout() {
  local seconds=$1 cmd=$2; shift 2
  case $cmd in
    xdotool) case $1 in
      getwindowgeometry) printf 'WINDOW=7\\nX=0\\nY=0\\nWIDTH=640\\nHEIGHT=384\\n';;
      getdisplaygeometry) printf '1280 800\\n';;
      *) return 98;; esac;;
    import) echo simulated-timeout >&2; return 124;;
    *) return 99;;
  esac
}
" out)
    (write-string (subseq source start (+ end 3)) out)
    (write-string "printf 'x11-screen-pause14-attempt01 640 384 1\\n' > \"$art/on-phase/capture-request\"
supplement_capture
" out))
  (uiop:run-program (list "bash" (namestring script)))
  (let* ((prefix (merge-pathnames "on-phase/x11-screen-pause14-attempt01" dir))
         (external (vttest-check:read-value (pathname (concatenate 'string (namestring prefix) ".external.sexp")))))
    (check (= 124 (getf external :exit)) :shell-failure-exit)
    (check (not (getf external :png)) :shell-failure-no-png)
    (check (getf external :identity-stable) :shell-identity-before-after)
    (dolist (suffix '(".ack" ".request.txt" ".capture.log" ".external.sexp"))
      (check (probe-file (pathname (concatenate 'string (namestring prefix) suffix))) :shell-retained))))
(format t "~&On-phase method: ~d deterministic checks passed; no GUI or PNG.~%" *checks*)
