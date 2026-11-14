(in-package #:lispterm.gpu.tests)

;;; gpu-wayland-first-resize: the complete host/PTY path on a private hardware
;;; GLES compositor. The window opens at 100x30 with a child that prints at once
;;; and then sleeps; tiling on a large output resizes it around its first frame.

(defparameter *first-resize-output* '(1680 1400)
  "Mode of the private headless output, in pixels.")
(defparameter *first-resize-gaps* '(("right" 25) ("bottom" 43))
  "Workspace gaps leaving the tiled surface of the reported failure.")
(defparameter *first-resize-surface* '(1655 1357)
  "Tiled surface size the compositor must configure, in pixels.")
(defparameter *first-resize-marker* "FIRST-RESIZE-READY")
(defparameter *first-resize-later-frames* 6
  "Frames requested after the first frame at the tiled size.")
(defvar *first-resize-trace* nil
  "Distinct published observations of the running host, newest first.")

(defun first-resize-swaymsg (&rest arguments)
  "Run swaymsg against the owned compositor's IPC socket only."
  (fixture-command (list* (format nil "SWAYSOCK=~A" (uiop:getenv "SWAYSOCK")) "swaymsg" arguments)))

(defun first-resize-write (directory name value)
  (with-open-file (out (merge-pathnames name directory) :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-readably* nil) (*print-pretty* t) (*print-right-margin* 110))
        (prin1 value out) (terpri out)))))

(defun first-resize-source-hashes ()
  "Hash the actual production files on disk, including dirty tracked sources."
  (let* ((names (uiop:run-program '("git" "ls-files" "-z" "--" "src/" "engine/" "scripts/" "bin/" "lispterm.asd")
                                  :directory *root* :input nil :output :string :error-output *error-output*))
         (files (remove "" (uiop:split-string names :separator '(#\Null)) :test #'equal)))
    (uiop:run-program (list* "sha256sum" "--" files) :directory *root* :input nil
                      :output :string :error-output *error-output*)))

(defun first-resize-dma-events (path)
  "Read only completed trace lines. The owner may still be writing the last line."
  (with-open-file (in path)
    (let ((*read-eval* nil))
      (loop for line = (read-line in nil nil) while line
            for event = (handler-case (read-from-string line) (end-of-file () nil))
            when event collect event))))

(defun first-resize-event-field (event key)
  (getf (cdddr event) key))

(defun first-resize-old-release-p (path)
  (let* ((events (first-resize-dma-events path))
         (initial (find :attach-commit events :key #'third))
         (serial (first-resize-event-field initial :buffer)))
    (and initial
         (find-if (lambda (event)
                    (and (eq :release (third event))
                         (eql serial (first-resize-event-field event :buffer))
                         (first-resize-event-field event :busy)
                         (not (first-resize-event-field event :destroyed)))) events))))

(defun first-resize-wire-commits (events commits surface)
  "Match each DMA record to its owned surface send within one presentation."
  (let ((previous-send 0))
    (loop for commit in commits
          for position = (position commit events :test #'eq)
          for enter = (find-if (lambda (event)
                                 (member (third event) '(:present-enter :present-exit)))
                               (reverse (subseq events 0 position)))
          for end = (find-if (lambda (event)
                               (member (third event) '(:present-enter :present-exit :attach-commit)))
                             (nthcdr (1+ position) events))
          collect
          (progn
            (check (and enter end (eq :present-enter (third enter))
                        (eq :present-exit (third end))
                        (< (first enter) (first commit) (first end)))
                   "DMA commit lacks an enclosing presentation: ~S ~S ~S" enter commit end)
            (let ((send (find-if
                         (lambda (event)
                           (and (< (first commit) (first event) (first end))
                                (eq :send (third event))
                                (eq :surface (first-resize-event-field event :interface))
                                (eql surface (first-resize-event-field event :object))
                                (eql 6 (first-resize-event-field event :opcode)))) events)))
              (check (and send (< previous-send (first send)))
                     "Missing distinct ordered wl_surface.commit on owned surface ~S in ~S .. ~S"
                     surface commit end)
              (setf previous-send (first send))
              send)))))

(defun first-resize-lifecycle-checks (directory surface)
  "Prove the trigger prefix with buffer identity and wire order, not totals."
  (let* ((events (first-resize-dma-events (merge-pathnames "first-resize-dma.sexp" directory)))
         (commits (remove-if-not (lambda (event) (eq :attach-commit (third event))) events))
         (initial (first commits)) (tiled (second commits))
         (serial (first-resize-event-field initial :buffer))
         (id (first-resize-event-field initial :id))
         (release (find-if (lambda (event)
                            (and (eq :release (third event))
                                 (eql serial (first-resize-event-field event :buffer))
                                 (first-resize-event-field event :busy)
                                 (not (first-resize-event-field event :destroyed)))) events))
         (destroy (find-if (lambda (event)
                            (and (eq :destroy (third event))
                                 (eql serial (first-resize-event-field event :buffer))
                                 (not (first-resize-event-field event :disconnected))
                                 (not (first-resize-event-field event :already-destroyed)))) events))
         (destroy-send (and destroy
                           (find-if (lambda (event)
                                      (and (> (first event) (first destroy))
                                           (eq :send (third event))
                                           (eql id (first-resize-event-field event :object))
                                           (eql 0 (first-resize-event-field event :opcode)))) events)))
         (third-commit (third commits))
         (present (and destroy
                       (find :present-enter (reverse (subseq events 0 (position destroy events))) :key #'third)))
         (late (and destroy-send
                    (find-if (lambda (event)
                               (and (> (first event) (first destroy-send))
                                    (eq :event (third event))
                                    (eql id (first-resize-event-field event :object))
                                    (eql 0 (first-resize-event-field event :opcode))
                                    (notany (lambda (created)
                                              (and (eq :created (third created))
                                                   (eql id (first-resize-event-field created :id))
                                                   (< (first destroy-send) (first created) (first event))))
                                            events))) events))))
    (check (>= (length commits) 3) "Missing three identified DMA commits: ~S" commits)
    (check (equal '(800 480) (first-resize-event-field initial :commit)) "Initial DMA commit: ~S" initial)
    (check (equal *first-resize-surface* (first-resize-event-field tiled :commit)) "Second DMA commit: ~S" tiled)
    (check (and serial id (not (eql serial (first-resize-event-field tiled :buffer)))
                release destroy destroy-send third-commit present
                (eql id (first-resize-event-field release :id))
                (eql id (first-resize-event-field destroy :id))
                (> (first-resize-event-field tiled :buffer-gen)
                   (first-resize-event-field initial :buffer-gen))
                (< (first initial) (first tiled) (first release) (first present)
                   (first destroy) (first destroy-send) (first third-commit))
                (not (first-resize-event-field destroy :busy))
                (= (first-resize-event-field initial :buffer-gen)
                   (first-resize-event-field destroy :buffer-gen)))
           "Missing ordered A commit -> tiled B commit -> release A -> third present collects/destroys idle A: ~S"
           (list initial tiled release present destroy destroy-send third-commit))
    (let ((wire-commits (first-resize-wire-commits events (list initial tiled third-commit) surface)))
      (first-resize-write directory "first-resize-lifecycle.sexp"
                          (list :initial initial :tiled tiled :release release :third-present present
                                :destroy destroy :destroy-send destroy-send :third-commit third-commit
                                :surface surface :wire-commits wire-commits :late-wire-event late)))
    (format t "FIRST-RESIZE lifecycle buffer ~D wire ~D: initial -> tiled -> release -> third-present destroy -> commit; late-event ~S~%"
            serial id (and late t))
    (unless late
      (format t "FIRST-RESIZE private compositor emitted no late event for destroyed A; old-handler rejection belongs to gpu-dma-release replay, not this native run.~%"))))

(defun first-resize-configure-output (directory)
  "Give the owned compositor its large output and tiling gaps before any window exists."
  (destructuring-bind (width height) *first-resize-output*
    (first-resize-swaymsg (format nil "output HEADLESS-1 mode ~Dx~D" width height))
    (loop for (side pixels) in *first-resize-gaps*
          do (first-resize-swaymsg (format nil "gaps ~A all set ~D" side pixels)))
    (let ((outputs nil))
      (fixture-wait (lambda ()
                      (setf outputs (first-resize-swaymsg "-t" "get_outputs" "-r"))
                      (and (search (format nil "\"width\": ~D" width) outputs)
                           (search (format nil "\"height\": ~D" height) outputs)))
                    "large private output mode")
      (with-open-file (out (merge-pathnames "outputs-large.json" directory)
                           :direction :output :if-exists :error)
        (write-string outputs out))
      (format t "FIRST-RESIZE output HEADLESS-1 ~Dx~D gaps ~S verified through owned IPC~%"
              width height *first-resize-gaps*))))

(defun first-resize-screen-text (host)
  (let* ((session (lispterm::host-session host))
         (screen (lispterm::session-screen session)))
    (sb-thread:with-mutex ((lispterm::session-lock session))
      (with-output-to-string (out)
        (dotimes (row (lispterm.engine:rows screen))
          (dotimes (column (lispterm.engine:columns screen))
            (write-char (or (lispterm.engine:cell-character (lispterm.engine:cell-at screen row column))
                            #\Space)
                        out)))))))

(defun first-resize-direct (status)
  (getf (getf status :geometry) :direct-presentation))

(defun first-resize-observation (status)
  (let ((geometry (getf status :geometry)) (direct (first-resize-direct status)))
    (list :frames (getf status :frames) :grid (list (getf status :columns) (getf status :rows))
          :pty (list (getf status :pty-columns) (getf status :pty-rows))
          :surface (list (getf geometry :buffer-width) (getf geometry :buffer-height))
          :display (list (getf geometry :actual-width) (getf geometry :actual-height))
          :committed (list (getf geometry :committed-buffer-width) (getf geometry :committed-buffer-height))
          :buffer-generation (getf geometry :buffer-generation)
          :configure-generation (getf geometry :configure-generation)
          :commit-generation (getf geometry :commit-generation)
          :frame-generation (getf geometry :frame-generation)
          :phase (getf geometry :phase) :reason (getf geometry :reason)
          :presents (getf direct :presents) :releases (getf direct :releases)
          :exports (getf direct :exports) :reuses (getf direct :reuses)
          :buffers (getf direct :buffers) :disabled-reason (getf direct :disabled-reason)
          :readbacks (getf geometry :readbacks))))

(defun first-resize-await (host description predicate &key (seconds 30))
  "Poll the published host status until PREDICATE accepts it, and return that status.
A recorded host error or a detached display fails at once with the evidence."
  (loop with deadline = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
        for status = (lispterm:status host)
        for errors = (lispterm:host-errors host)
        for observation = (first-resize-observation status)
        do (unless (equal observation (first *first-resize-trace*))
             (push observation *first-resize-trace*))
           (check (null errors) "Host error while awaiting ~A: ~S~%observed ~S"
                  description errors observation)
           (check (eq :attached (getf status :state)) "Host is ~S while awaiting ~A: ~S"
                  (getf status :state) description observation)
           (when (funcall predicate status) (return status))
           (check (< (get-internal-real-time) deadline) "Timed out awaiting ~A: ~S"
                  description observation)
           (sleep 0.005)))

(defun first-resize-grid (status)
  "Columns and rows of the tiled surface for the cell size HOST published, or NIL."
  (let* ((geometry (getf status :geometry))
         (cell-width (getf geometry :cell-width)) (cell-height (getf geometry :cell-height)))
    (when (and cell-width cell-height)
      (list (floor (first *first-resize-surface*) cell-width)
            (floor (second *first-resize-surface*) cell-height)))))

(defun first-resize-tiled-p (status)
  "True once engine, PTY record, and surface all have the tiled geometry."
  (let ((geometry (getf status :geometry))
        (grid (first-resize-grid status)))
    (and grid
         (equal *first-resize-surface*
                (list (getf geometry :buffer-width) (getf geometry :buffer-height)))
         (equal grid (list (getf status :columns) (getf status :rows)))
         (equal grid (list (getf status :pty-columns) (getf status :pty-rows))))))

(defun first-resize-frame-settled-p (status)
  "True when the latest commit has the display's size and its frame callback arrived."
  (let ((geometry (getf status :geometry)))
    (and (plusp (or (getf (first-resize-direct status) :presents) 0))
         (plusp (or (getf geometry :frame-generation) 0))
         (not (getf geometry :frame-pending-p))
         (eql (getf geometry :committed-buffer-width) (getf geometry :actual-width))
         (eql (getf geometry :committed-buffer-height) (getf geometry :actual-height)))))

(defun first-resize-later-frame (host index)
  "Request one more frame and wait for its commit and compositor frame callback."
  (let* ((before (first-resize-await host "an idle surface" #'first-resize-frame-settled-p))
         (frames (getf before :frames))
         (presents (getf (first-resize-direct before) :presents))
         (callbacks (getf (getf before :geometry) :frame-generation)))
    (lispterm:invalidate host)
    (first-resize-await host (format nil "later frame ~D" index)
      (lambda (status)
        (and (> (getf status :frames) frames)
             (> (getf (first-resize-direct status) :presents) presents)
             (> (getf (getf status :geometry) :frame-generation) callbacks)
             (first-resize-frame-settled-p status))))))

(defun first-resize-final-checks (host status)
  (let* ((geometry (getf status :geometry)) (direct (first-resize-direct status))
         (driver (getf geometry :driver))
         (grid (first-resize-grid status))
         (pty (lispterm::session-pty (lispterm::host-session host))))
    (check (and (eq :attached (getf status :state)) (getf status :worker-alive-p))
           "Host is not attached with a live owner: ~S" status)
    (check (and (zerop (getf status :error-count)) (null (lispterm:host-errors host)))
           "Host recorded display errors: ~S" (lispterm:host-errors host))
    (check (eq :wayland (getf status :backend)) "Effective backend is ~S" (getf status :backend))
    (check (and (eq :gpu (getf geometry :renderer)) (null (getf geometry :renderer-fallback-reason)))
           "Renderer is ~S, fallback ~S" (getf geometry :renderer) (getf geometry :renderer-fallback-reason))
    (check (and (getf driver :device-associated-p) (not (getf driver :software-p)))
           "Renderer is not hardware GLES: ~S" driver)
    (check (first-resize-tiled-p status) "Tiled geometry was lost: ~S" (first-resize-observation status))
    (check (not (equal grid '(100 30))) "Grid did not change from 100x30")
    (check (equal (reverse grid) (multiple-value-list (lispterm.pty:pty-window-size pty)))
           "Kernel PTY size ~S differs from grid ~S"
           (multiple-value-list (lispterm.pty:pty-window-size pty)) grid)
    (check (and (plusp (getf geometry :buffer-generation)) (plusp (getf geometry :configure-generation)))
           "Surface generation did not advance: buffer ~S configure ~S"
           (getf geometry :buffer-generation) (getf geometry :configure-generation))
    (check (first-resize-frame-settled-p status) "Latest frame is not settled at the tiled size: ~S"
           (first-resize-observation status))
    (check (null (getf direct :disabled-reason)) "Direct presentation disabled: ~S" (getf direct :disabled-reason))
    (check (> (getf direct :presents) *first-resize-later-frames*)
           "Only ~S direct presents for ~D later frames" (getf direct :presents) *first-resize-later-frames*)
    (check (>= (getf direct :releases) 3) "Only ~S actual wl_buffer.release events" (getf direct :releases))
    (check (= (getf direct :presents) (getf direct :gpu-finishes)) "Missing GPU completion barrier: ~S" direct)
    ;; The renderer publishes :readbacks only after its first readback.
    (check (eql 0 (getf geometry :readbacks 0)) "Direct presentation performed ~S GPU readbacks"
           (getf geometry :readbacks))
    (check (null (lispterm.display::display-framebuffer-storage (lispterm::host-display host)))
           "Direct presentation allocated a CPU composed/readback frame")
    (check (and (eq :open (getf status :transport-state)) (eq :running (getf status :process-state)))
           "Sleeping child is not running on an open PTY: transport ~S process ~S"
           (getf status :transport-state) (getf status :process-state))
    t))

(defun first-resize-close-checks (host threads)
  "Close HOST the ordinary way and require the child reaped and every owner gone."
  (let* ((pty (lispterm::session-pty (lispterm::host-session host)))
         (process (lispterm.pty:pty-process pty))
         (pid (lispterm.pty:process-pid process)))
    (check (eq :running (lispterm.pty:process-poll process)) "Child ~D ended before close" pid)
    (check (eq host (lispterm:close host)) "Ordinary close did not return its host")
    (let ((status (lispterm:status host)))
      (check (and (eq :closed (getf status :state)) (not (getf status :worker-alive-p)))
             "Closed host retains an owner: ~S" status))
    (check (null (lispterm:host-errors host)) "Close recorded host errors: ~S" (lispterm:host-errors host))
    (check (null (lispterm:status)) "Closed host is still registered")
    (check (not (lispterm.pty:pty-open-p pty)) "PTY survived close")
    (check (member (lispterm.pty:process-status process) '(:exited :signaled))
           "Child ~D was not reaped: ~S" pid (lispterm.pty:process-status process))
    (check (not (probe-file (format nil "/proc/~D/stat" pid))) "Child ~D survived close" pid)
    (lispterm.gpu::require-no-live-contexts)
    (check (zerop (hash-table-count lispterm.display.wayland::*dma-protocols*))
           "Direct protocol state survived close")
    (check (notany #'sb-thread:thread-alive-p (set-difference (sb-thread:list-all-threads) threads))
           "Host thread survived close")
    (format t "FIRST-RESIZE close reaped child ~D as ~S exit ~S, no contexts, protocols, or threads left~%"
            pid (lispterm.pty:process-status process) (lispterm.pty:process-exit-code process))))

(defun first-resize-abandon (host)
  "Release HOST after a failed check without hiding that failure."
  (handler-case (lispterm:close host)
    (error (condition)
      (format *error-output* "FIRST-RESIZE cleanup close failed: ~A~%" condition)
      (let ((pty (lispterm::session-pty (lispterm::host-session host))))
        (when (lispterm.pty:pty-open-p pty)
          (handler-case (lispterm.pty:close-pty pty)
            (error (condition)
              (format *error-output* "FIRST-RESIZE cleanup PTY close failed: ~A~%" condition))))))))

(defun first-resize-session (directory)
  (let ((threads (sb-thread:list-all-threads)) (*first-resize-trace* nil)
        (host nil) (closed-p nil))
    (unwind-protect
         (progn
           ;; The launcher's own defaults: automatic backend and renderer, 100x30.
           (setf host (lispterm:open :backend :auto :renderer :auto :columns 100 :rows 30
                                     :title "Lispterm first resize"
                                     :argv (list "sh" "-c"
                                                 (format nil "printf '~A\\r\\n'; exec sleep 120"
                                                         *first-resize-marker*))))
           (first-resize-await host "immediate child output"
             (lambda (status)
               (and (plusp (getf status :bytes-received))
                    (search *first-resize-marker* (first-resize-screen-text host)))))
           (first-resize-await host "tiling resize of grid, PTY, and surface" #'first-resize-tiled-p)
           (let ((status (first-resize-await host "first settled frame at the tiled size"
                           (lambda (status)
                             (and (first-resize-tiled-p status) (first-resize-frame-settled-p status))))))
             (format t "FIRST-RESIZE first tiled frame ~S~%" (first-resize-observation status)))
            ;; The next invalidation must collect the old-size buffer after its
            ;; actual first release, not merely advance a presentation counter.
            (first-resize-await host "identified first-size buffer release"
              (lambda (status)
                (declare (ignore status))
                (first-resize-old-release-p (merge-pathnames "first-resize-dma.sexp" directory))))
           (loop for index from 1 to *first-resize-later-frames*
                 do (first-resize-later-frame host index))
           (let ((status (first-resize-await host "actual buffer releases"
                           (lambda (status)
                             (and (>= (or (getf (first-resize-direct status) :releases) 0) 3)
                                  (first-resize-frame-settled-p status))))))
             (check (search *first-resize-marker* (first-resize-screen-text host))
                    "Child output was lost by the resize")
             (first-resize-final-checks host status)
             (first-resize-write directory "first-resize-status.sexp" status)
             (format t "FIRST-RESIZE attached ~S~%" (first-resize-observation status)))
             (first-resize-lifecycle-checks
              directory (lispterm.display.wayland::wayland-state-surface
                         (lispterm.display.wayland::window-protocol (lispterm::host-display host))))
           (first-resize-close-checks host threads)
           (setf closed-p t))
      (first-resize-write directory "first-resize-trace.sexp" (reverse *first-resize-trace*))
      (format t "FIRST-RESIZE trace ~D observations in ~A~%" (length *first-resize-trace*)
              (merge-pathnames "first-resize-trace.sexp" directory))
      (when (and host (not closed-p)) (first-resize-abandon host)))
    t))

(defun gpu-wayland-first-resize-check ()
  (dolist (name '("DISPLAY" "WAYLAND_DISPLAY" "WAYLAND_SOCKET"))
    (check (not (uiop:getenv name)) "Inherited endpoint ~A" name))
  (call-with-direct-compositor
   (lambda (directory)
     (first-resize-configure-output directory)
     (let ((before (first-resize-source-hashes))
           (old-stream lispterm.display.wayland::*dma-trace-stream*)
           (old-hook lispterm.display.wayland::*wire-trace*)
           (old-lines lispterm.display.wayland::*dma-trace-lines*))
       (with-open-file (out (merge-pathnames "production-before.sha256" directory)
                            :direction :output :if-exists :error)
         (write-string before out))
       (with-open-file (stream (merge-pathnames "first-resize-dma.sexp" directory)
                               :direction :output :if-exists :error)
         ;; Set the opt-in globals explicitly: the host owns a separate thread.
         (unwind-protect
              (progn
                (setf lispterm.display.wayland::*dma-trace-stream* stream
                      lispterm.display.wayland::*dma-trace-lines* 0
                      lispterm.display.wayland::*wire-trace* #'lispterm.display.wayland::dma-trace-wire)
                (first-resize-session directory))
           (setf lispterm.display.wayland::*dma-trace-stream* old-stream
                 lispterm.display.wayland::*wire-trace* old-hook
                 lispterm.display.wayland::*dma-trace-lines* old-lines)
           (let ((after (first-resize-source-hashes)))
             (with-open-file (out (merge-pathnames "production-after.sha256" directory)
                                  :direction :output :if-exists :error)
               (write-string after out))
             (check (equal before after) "Production source changed during first-resize check")))))
     (format t "FIRST-RESIZE source hashes are disk observations; the runner loaded Lisp before gate entry. Final frozen-tree rerun required after concurrent writers finish.~%")))
  t)
(register-check "gpu-wayland-first-resize" #'gpu-wayland-first-resize-check)
