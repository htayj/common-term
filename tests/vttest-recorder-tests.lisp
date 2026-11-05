;;;; Compile recorder definitions, then exercise boundaries with disposable fakes.
;;;; No display, PTY, instrumentation installation, or vttest process is opened.
(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system "lispterm")
(load (merge-pathnames "vttest-protocol.lisp" *load-truename*))
(defparameter *recorder* (merge-pathnames "vttest-gate.lisp" *load-truename*))
;; Read every form with all real packages present. Only definitions are installed;
;; skip instrumentation LETs and the top-level runner, which are GUI operations.
(let ((*load-truename* *recorder*))
  (with-open-file (s *recorder*)
    (loop for form = (read s nil :eof) until (eq form :eof)
          when (member (car form) '(defun defparameter defvar))
            do (if (eq (car form) 'defun)
                   (progn (eval form) (compile (second form)))
                   (eval form)))))
(defparameter *test-count* 0)
(defun verify (value name)
  (unless value (error "FAILED ~a" name))
  (incf *test-count*) (format t "PASS ~a~%" name))
(defun errors-p (thunk) (handler-case (progn (funcall thunk) nil) (error () t)))
(defun with-functions (bindings thunk)
  (let ((old (mapcar (lambda (pair) (cons (car pair) (symbol-function (car pair)))) bindings)))
    (unwind-protect (progn (dolist (pair bindings) (setf (symbol-function (car pair)) (cdr pair))) (funcall thunk))
      (dolist (pair old) (setf (symbol-function (car pair)) (cdr pair))))))
(setf *art* (uiop:ensure-directory-pathname
             (or (uiop:getenv "LISPTERM_HARNESS_TEST_ARTIFACTS") ".build/evidence/vttest-harness85/recorder-fixtures/")))
(ensure-directories-exist (artifact "placeholder"))
(verify (= 20 (length *cases*)) "selected VT100 case inventory is explicit")
(verify (menu-p "Test of cursor movements Enter choice number" (menu-title nil)) "main menu identity")
(verify (not (menu-p "Keyboard Tests Enter choice number" (menu-title nil))) "wrong submenu not a main return")
(let ((text "Push <RETURN>")
      (s '(:frames 1 :bytes-received 1 :columns 80 :rows 24 :pty-columns 80 :pty-rows 24
           :engine-geometry-generation 0 :error-count 0
           :geometry (:request-id nil :phase :idle :epoch 0 :display-epoch 1 :metric-generation 0
                      :cell-width 9 :cell-height 18 :columns 80 :rows 24
                      :actual-width 720 :actual-height 432 :presentation 0 :target-presentation nil
                      :buffer-scale 1 :configured-p t :frame-pending-p nil
                      :committed-width 720 :committed-height 432 :committed-buffer-width 720
                      :committed-buffer-height 432 :buffer-width 720 :buffer-height 432
                      :commit-generation 1 :frame-generation 1)
           :process-state :running :backend :x11 :input-pending 0)))
  (with-functions
      (list (cons 'grid-text (lambda (host) (declare (ignore host)) text))
            (cons 'lispterm:status (lambda (&optional host) (declare (ignore host)) (copy-tree s)))
            (cons 'lispterm:host-errors (lambda (host) (declare (ignore host)) nil))
            (cons 'lispterm:invalidate (lambda (host) (declare (ignore host)) (incf (getf s :frames))
                    (incf (getf (getf s :geometry) :commit-generation))
                    (incf (getf (getf s :geometry) :frame-generation)))))
    (lambda ()
      (setf *output-count* 1)
      (verify (equal text (wait-screen :fake :seconds .5 :after 0 :predicate (lambda (x) (search "Push <RETURN>" x))))
              "fresh stable expected boundary succeeds")
      (verify (errors-p (lambda () (wait-screen :fake :seconds .05 :after 1 :predicate (constantly t))))
              "stale prompt without new PTY bytes times out")
      (setf (getf s :input-pending) 1)
      (verify (errors-p (lambda () (wait-screen :fake :seconds .05 :after 0 :predicate (constantly t))))
              "fresh prompt with undrained input cannot advance")
      (setf (getf s :input-pending) 0)
      (let ((*output-count* *output-count*))
        (with-functions (list (cons 'grid-text (lambda (host) (declare (ignore host)) (incf *output-count*) text)))
          (lambda ()
            (verify (errors-p (lambda () (wait-screen :fake :seconds .05 :after 0 :predicate (constantly t))))
                    "changing output cannot advance an unstable prompt"))))
      (verify (errors-p (lambda () (wait-screen :fake :seconds .05 :after 0 :predicate (constantly nil))))
              "unrecognized boundary deadline fails")
      (setf (getf s :process-state) :exited)
      (verify (errors-p (lambda () (wait-screen :fake :predicate (constantly t)))) "early exit is not prompt success")
      (setf (getf s :process-state) :running)
      (verify (getf (frame-barrier :fake) :status) "capture requires frame advance after target PTY bytes")
      (setf (getf s :backend) :wayland *wayland-ack-output* 1)
      (verify (getf (frame-barrier :fake) :status) "native target commit callback acknowledged")
      (setf *wayland-ack-output* 0)
      (with-functions (list (cons 'deadline (lambda (seconds) (declare (ignore seconds)) (+ (get-internal-real-time) (* .05 internal-time-units-per-second)))))
        (lambda ()
          (verify (errors-p (lambda () (frame-barrier :fake))) "native unacknowledged target frame fails closed")
          (setf *wayland-ack-output* 1)
          (let ((saved (copy-tree s)))
            (unwind-protect
                (progn
                  (let ((g (getf s :geometry)))
                    (setf (getf s :engine-geometry-generation) 7
                          (getf g :request-id) 7 (getf g :phase) :settled
                          (getf g :target-presentation) 0
                          (getf g :requested-columns) 80 (getf g :requested-rows) 24
                          (getf g :requested-width) 720 (getf g :requested-height) 432)
                    (setf (getf s :geometry) g)
                    (verify (getf (frame-barrier :fake) :status)
                            "settled request needs subsequent redraw, presentation may be unchanged"))
                  (setf s (copy-tree saved))
                  (let ((*desired-scale* 2) (samples 0))
                    (with-functions
                        (list (cons 'lispterm:status
                                    (lambda (&optional host) (declare (ignore host))
                                      (when (> (incf samples) 1)
                                        (let ((g (getf s :geometry)))
                                          (setf (getf g :buffer-scale) 2
                                                (getf g :committed-buffer-width) 1440
                                                (getf g :committed-buffer-height) 864
                                                (getf g :buffer-width) 1440 (getf g :buffer-height) 864)))
                                      (copy-tree s))))
                      (lambda () (verify (getf (frame-barrier :fake) :status)
                                         "scale2 waits for actual initial surface-enter scale"))))
                  (setf s (copy-tree saved))
                  (dolist (phase '(:refused :timed-out :cancelled :superseded))
                    (setf (getf (getf s :geometry) :phase) phase)
                    (verify (errors-p (lambda () (frame-barrier :fake)))
                            (format nil "terminal ~a cannot enter normal capture" phase)))
                  (setf s (copy-tree saved))
                  (with-functions (list (cons 'lispterm:invalidate (lambda (host) (declare (ignore host)) nil)))
                    (lambda ()
                      (verify (errors-p (lambda () (frame-barrier :fake)))
                              "settlement target without invalidate-frame progress fails")))
                  (with-functions
                      (list (cons 'lispterm:invalidate
                                  (lambda (host) (declare (ignore host))
                                    (incf (getf s :frames)) (incf (getf (getf s :geometry) :epoch)))))
                    (lambda () (verify (errors-p (lambda () (frame-barrier :fake)))
                                       "retry epoch changed after settled baseline fails")))
                  (setf s (copy-tree saved))
                  (let ((samples 0))
                    (with-functions
                        (list (cons 'lispterm:status
                                    (lambda (&optional host) (declare (ignore host))
                                      (let ((snapshot (copy-tree s)))
                                        (when (= 1 (incf samples)) (setf (getf snapshot :geometry) nil))
                                        snapshot))))
                      (lambda () (verify (getf (frame-barrier :fake) :status)
                                         "initial NIL geometry waits for attached display")))))
              (setf s saved)))
          (setf text "wrong screen" *expected* '("reports-dsr") *attempted* nil *completed* nil)
          (with-functions
              (list (cons 'lispterm:open (lambda (&rest args) (declare (ignore args)) :fake))
                    (cons 'lispterm:close (lambda (host) (declare (ignore host)) nil)))
            (lambda ()
              (run-case :x11 "reports-dsr" '(6 3) 1)
              (let ((r (vttest-check:read-value (artifact "reports-dsr.result.sexp"))))
                (verify (and (getf r :failure) (not (getf r :inventory-ok))
                             (not (getf r :returned-to-menu)) (null *completed*))
                        "broken recorder case cannot enter completed inventory")))))))))

;; Run the real recorder dispatch against scripted current grids. Host I/O,
;; captures and persistence are fakes; no speculative input is hidden.
(defun keyboard-grid (title &optional hold-row)
  (with-output-to-string (out)
    (dotimes (row 24)
      (format out "~80a~%"
              (cond ((= row 20) (format nil "<~a>" title))
                    ((= row 21) (if (member title '("ANSI / Cursor key mode RESET" "ANSI / Cursor key mode SET" "VT52 Mode") :test #'equal)
                                   "Press each cursor key. Finish with TAB."
                                   "Press each function key. Finish with TAB."))
                    ((eql row hold-row) "Push <RETURN>") (t ""))))))
(defun scripted-case (label path holds screens &key fail-key)
  (let ((inputs nil) (captures nil) (exited nil) (records (make-hash-table :test #'equal))
        (*expected* (list label)) (*attempted* nil) (*completed* nil))
    (with-functions
        (list (cons 'lispterm:open (lambda (&rest args) (declare (ignore args)) :fake))
              (cons 'lispterm:close (lambda (host) (declare (ignore host))))
              (cons 'lispterm:host-errors (lambda (host) (declare (ignore host)) nil))
              (cons 'lispterm:status
                    (lambda (&optional host) (declare (ignore host))
                      (list :input-pending 0 :process-state (if exited :exited :running)
                            :transport-state (if exited :hangup :attached))))
              (cons 'record-value (lambda (name value) (setf (gethash name records) value)))
              (cons 'vttest-check:read-value (lambda (path) (declare (ignore path)) 0))
              (cons 'event (lambda (&rest args) (declare (ignore args))))
              (cons 'wait-screen
                    (lambda (host &key seconds (after -1) predicate)
                      (declare (ignore host seconds))
                      (unless screens (error "No next scripted output"))
                      (incf *output-count*)
                      (let ((text (pop screens)))
                        (unless (and (> *output-count* after) (funcall predicate text))
                          (error "Unrecognized scripted grid")) text)))
              (cons 'send-line
                    (lambda (host choice) (declare (ignore host))
                      (push choice inputs)
                      (when (string= *phase* "exit") (setf exited t))))
              (cons 'send (lambda (host text) (declare (ignore host)) (push text inputs)))
              (cons 'native-key (lambda (host key) (declare (ignore host))
                                  (when (equal key fail-key) (error "Scripted native delivery failure"))
                                  (push key inputs)))
              (cons 'capture (lambda (host name) (declare (ignore host)) (push name captures))))
      (lambda () (run-case :x11 label path holds)))
    (values (gethash (format nil "~a.result.sexp" label) records)
            (reverse inputs) (reverse captures))))
(defparameter *main-grid* "Test of cursor movements Enter choice number")
(defparameter *keyboard-menu-grid* "Keyboard Tests Enter choice number")
(defun screen-grid (screen)
  (with-output-to-string (out)
    (dotimes (row (lispterm.engine:rows screen))
      (dotimes (column (lispterm.engine:columns screen))
        (write-char (or (lispterm.engine:cell-character
                         (lispterm.engine:cell-at screen row column)) #\Space) out))
      (terpri out))))
(defun replay-grid (wire &optional (cuts (list (length wire))))
  (let ((screen (lispterm.engine:make-screen)) (start 0))
    (dolist (end cuts)
      (lispterm.engine:feed screen (subseq wire start end))
      (setf start end))
    (screen-grid screen)))
(defun retained-wire (name)
  (with-open-file (in (merge-pathnames (format nil "fixtures/vttest-~a.raw" name) *load-truename*)
                      :element-type '(unsigned-byte 8))
    (let ((wire (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence wire in)
      wire)))
;; Exact PTY output from vttest20251205, X11 failure root
;; .build/evidence/vttest-x11-scale1-v3.TbGKKv. These are replay inputs,
;; not successful native-run or visual-acceptance receipts.
(defparameter *bug-menu-grid* (replay-grid (retained-wire "bug-menu")))
(defparameter *cursor-titles*
  '("ANSI / Cursor key mode RESET" "ANSI / Cursor key mode SET" "VT52 Mode"))
(defparameter *keypad-titles*
  '("ANSI Numeric mode" "ANSI Application mode" "VT52 Numeric mode" "VT52 Application mode"))
(let ((failures nil))
  (flet ((check (value name)
           (if value (verify t name)
               (progn (push name failures) (format t "REGRESSION FAIL ~a~%" name)))))
    (dolist (spec '(("keyboard-cursor" (5 2) *cursor-titles* ("Up" "Down" "Left" "Right"))
                    ("keyboard-keypad" (5 3) *keypad-titles* ("KP_1" "KP_2" "KP_Enter"))))
      (destructuring-bind (label path title-variable keys) spec
        (let* ((titles (symbol-value title-variable))
               (phases (mapcar #'keyboard-grid titles))
               (final (keyboard-grid (car (last titles)) 22))
               (prefix (list *main-grid* *keyboard-menu-grid*)))
          (multiple-value-bind (result inputs captures)
              (scripted-case label path 1
                             (append prefix phases (list final *keyboard-menu-grid* *main-grid*)))
            (check (and (getf result :inventory-ok) (= 1 (getf result :pauses))
                        (= (length titles) (getf result :key-phases))
                        (equal inputs (append path
                                              (loop repeat (length titles) append (append keys '("Tab")))
                                              (list (string #\Return) 0 0)))
                        (= (1+ (* (length titles) (length keys))) (length captures)))
                   (format nil "~a final dual-prompt is one hold after exact keys/TABs" label)))
          (dolist (bad (list (list "early hold" (list final) 0 0)
                            (list "wrong first mode" (list (second phases)) 0 0)
                            (list "skipped mode" (list (first phases) (third phases)) 1 0)
                            (list "duplicate mode" (list (first phases) (first phases)) 1 0)
                            (list "extra mode" (append phases (list (first phases))) (length titles) 0)
                            (list "missing hold" (append phases (list (car (last phases)))) (length titles) 0)
                            (list "wrong hold row" (append phases (list (keyboard-grid (car (last titles)) 19))) (length titles) 0)
                            (list "wrong final caption" (append phases (list (keyboard-grid (first titles) 22))) (length titles) 0)
                            (list "duplicate hold" (append phases (list final final)) (length titles) 1)
                            (list "wrong return menu" (append phases (list final *main-grid*)) (length titles) 1)))
            (multiple-value-bind (result inputs)
                (scripted-case label path 1 (append prefix (second bad)))
              (check (and (getf result :failure) (not (getf result :inventory-ok))
                          (equal inputs (append path
                                                (loop repeat (third bad) append (append keys '("Tab")))
                                                (loop repeat (fourth bad) collect (string #\Return)))))
                     (format nil "~a rejects ~a without speculative input" label (first bad))))))))
    (check (menu-p *bug-menu-grid* (menu-title '(9 2)))
           "surviving known-bugs identity and exact menu boundary")
    (when failures (error "Recorder contract regressions: ~s" (reverse failures)))))

;; All bug paths must reach their body and return through this same submenu.
(dolist (spec (remove-if-not (lambda (spec) (= 9 (first (second spec)))) *cases*))
  (destructuring-bind (label path holds) spec
    (let* ((custom (if (string= label "bug-d") '("Enter 0 to exit") nil))
           (pauses (loop for n from 1 to holds
                         collect (format nil "~a Push <RETURN>" (vttest-check:pause-anchor label n)))))
      (multiple-value-bind (result inputs captures)
          (scripted-case label path holds
                         (append (list *main-grid* *bug-menu-grid*) custom pauses
                                 (list *bug-menu-grid* *main-grid*)))
        (verify (and (getf result :inventory-ok) (= holds (getf result :pauses))
                     (equal inputs (append path (when custom '(0))
                                           (loop repeat holds collect (string #\Return)) '(0 0)))
                     (= (+ holds (length custom)) (length captures)))
                (format nil "~a exact choices/holds, D uses 0CR, exact return and exit" label))))))
(dolist (wrong (list *main-grid* *keyboard-menu-grid*
                     "Choose bug test number Enter choice number (0 - 9): "
                     (subseq *bug-menu-grid* 0 (search "Enter choice" *bug-menu-grid*))
                     (concatenate 'string
                                  (subseq *bug-menu-grid* 0 (search "Enter choice" *bug-menu-grid*))
                                  "Enter choice number (0 - 8): ")))
  (multiple-value-bind (result inputs)
      (scripted-case "bug-b" '(9 2) 2 (list *main-grid* wrong))
    (verify (and (getf result :failure) (equal inputs '(9)))
            "wrong/incomplete bug menu sends no speculative second choice")))

(let* ((esc (string #\Escape))
       ;; Source-defined redraw: an introductory row survives, row-10 heading
       ;; is cleared by menu2's CUP9/ED0, then choices and final prompt appear.
       (wire (map '(vector (unsigned-byte 8)) #'char-code
                  (format nil "~a[2J~a[1;1HTest of known bugs in the DEC VT100 series.~a[10;1HChoose bug test number~a[9;1H~a[0J0. Exit to main menu~c~c2. Bug B: Scrolling region~c~c9. Funny scroll regions~c~cEnter choice number (0 - 9): "
                          esc esc esc esc esc #\Return #\Newline #\Return #\Newline #\Return #\Newline)))
       (expected (replay-grid wire)))
  (verify (and (menu-p expected (menu-title '(9 2)))
               (not (search "Choose bug test number" expected)))
          "rendered bug menu recognizes surviving identity, not erased heading")
  (verify (loop for split from 0 to (length wire)
                always (equal expected (replay-grid wire (list split (length wire)))))
          "bug-menu CUP/ED redraw replay is invariant at every byte split")
  (verify (equal expected (replay-grid wire (loop for end from 1 to (length wire) collect end)))
          "bytewise bug-menu redraw replay"))
(let* ((wire (retained-wire "bug-menu")) (expected (replay-grid wire)))
  (verify (and (menu-p expected (menu-title '(9 2)))
               (not (search "Choose bug test number" expected)))
          "retained 1873-byte bug output erases heading and preserves menu identity")
  (verify (loop for split from 0 to (length wire)
                always (equal expected (replay-grid wire (list split (length wire)))))
          "retained bug output is invariant at every byte split")
  (verify (equal expected (replay-grid wire (loop for end from 1 to (length wire) collect end)))
          "retained bug output bytewise replay"))
(dolist (spec '(("keyboard-cursor" 3) ("keyboard-keypad" 4)))
  (destructuring-bind (label phases) spec
    (let* ((wire (retained-wire label)) (expected (replay-grid wire)))
      (verify (and (search "Press each" expected)
                   (keyboard-hold-p expected label phases 0)
                   (not (keyboard-hold-p expected label (1- phases) 0))
                   (not (keyboard-hold-p expected label phases 1)))
              (format nil "retained ~a dual-prompt admits only exact final hold" label))
      (dolist (size '(1 2 7 64))
        (verify (equal expected
                       (replay-grid wire (append (loop for end from size below (length wire) by size collect end)
                                                 (list (length wire)))))
                (format nil "retained ~a replay in ~d-byte chunks" label size)))
      (multiple-value-bind (result inputs)
          (scripted-case label (if (= phases 3) '(5 2) '(5 3)) 1
                         (append (list *main-grid* *keyboard-menu-grid*)
                                 (mapcar #'keyboard-grid (keyboard-titles label))
                                 (list expected *keyboard-menu-grid* *main-grid*)))
        (verify (and (getf result :inventory-ok) (= 1 (count (string #\Return) inputs :test #'equal)))
                (format nil "retained ~a final grid completes real recorder dispatch" label))))))
(dolist (spec '(("keyboard-cursor" (5 2) "Down" "Up")
                ("keyboard-keypad" (5 3) "KP_2" "KP_1")
                ("keyboard-cursor" (5 2) "Tab" "Right")
                ("keyboard-keypad" (5 3) "Tab" "KP_Enter")))
  (destructuring-bind (label path failed-key last-key) spec
    (multiple-value-bind (result inputs)
        (scripted-case label path 1
                       (list *main-grid* *keyboard-menu-grid* (keyboard-grid (first (keyboard-titles label))))
                       :fail-key failed-key)
      (verify (and (getf result :failure) (not (getf result :inventory-ok))
                   (equal (car (last inputs)) last-key)
                   (not (member "Tab" inputs :test #'equal))
                   (not (member (string #\Return) inputs :test #'equal)))
              (format nil "~a failed ~a sends no speculative TAB/RETURN" label failed-key)))))
(format t "~d recorder boundary checks passed; definitions compiled with real packages, no GUI launched~%" *test-count*)
