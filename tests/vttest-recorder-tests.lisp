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
(format t "~d recorder boundary checks passed; definitions compiled with real packages, no GUI launched~%" *test-count*)
