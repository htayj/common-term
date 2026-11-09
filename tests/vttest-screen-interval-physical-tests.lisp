;;;; X11 screen-interval controls over physical comparison roots. Each X11 root
;;;; is a shared-fixture pause rebound to one X11 bracket, so the screen
;;;; comparison contract and VALIDATE-SCREEN-INTERVAL judge the same files.
(require :sb-posix)
(dolist (f '("vttest-screen-native-contract" "vttest-screen-driver"
             "vttest-physical-contract-test-fixtures"))
  (load (merge-pathnames (concatenate 'string f ".lisp") *load-truename*)))
(defpackage :vttest-screen-interval-physical-tests (:use :cl) (:export :run-tests))
(in-package :vttest-screen-interval-physical-tests)

(defvar *pass* 0)
(defvar *log* nil)
(defun ok (label test) (unless test (error "Control failed: ~s" label)) (incf *pass*))

(defun x11-geometry ()
  "Settled X11 GetGeometry fields with the producer's duplicated keys."
  (list :request-id 7 :display-epoch 2 :epoch 3 :metric-generation 0
        :cell-width 8 :cell-height 16 :phase :settled :reason :server-compatible
        :requested-columns 80 :requested-rows 24 :deadline 12345
        :requested-width 640 :requested-height 384 :actual-width 640 :actual-height 384
        :columns 80 :rows 24 :presentation 10 :target-presentation 10
        :columns 80 :rows 24 :width 640 :height 384 :cell-width 8 :cell-height 16
        :backend :x11 :configure-sequence nil :window-id 123 :depth 24 :bits-per-pixel 32))
(defun x11-status (frames)
  (list :id 1 :state :attached :backend :x11 :worker-alive-p t :error-count 0
        :rows 24 :columns 80 :engine-geometry-generation 7 :pty-rows 24 :pty-columns 80
        :geometry (x11-geometry) :frames frames :blink-phase t :blink-visible t
        :bytes-received 100 :transport-state :open :process-state :running
        :child-pid 456 :input-pending 0 :input-limit 65536))

(defun stem (n suffix) (format nil "recorder/pause-~2,'0d-~a" n suffix))
(defun file (root n suffix) (concatenate 'string root (stem n suffix)))
(defun rewrite (path value)
  (delete-file path)
  (vttest-screen-native-contract:write-data path value))
(defun bracket (root n)
  (mapcar (lambda (suffix) (vttest-screen-native-contract:read-data (file root n suffix)))
          vttest-physical-contract-tests::+bracket+))

(defun x11-root (n scale &key (window 123) (ack -1))
  "Fixture pause N at SCALE rebound to an X11 bracket and request; return check, root."
  (multiple-value-bind (check root)
      (vttest-physical-contract-tests::build
       :screen n scale
       :native (lambda (p) (setf (getf p :backend) :x11 (getf p :window-id) window
                                 (getf p :ack-output) ack)
                 p))
    (let ((ready (x11-status 10))
          (record (vttest-screen-native-contract:read-data (file root n "comparison.sexp"))))
      (loop for value in (list (x11-status 9) ready ready ready)
            for suffix in vttest-physical-contract-tests::+bracket+
            do (rewrite (file root n suffix) value))
      (rewrite (file root n "request.sexp")
               (list :status ready :pause n :target 100 :ack-output -1 :surface-id 8))
      (setf (getf record :bracket)
            (mapcar (lambda (suffix) (vttest-screen-native-contract:reference root (stem n suffix)))
                    vttest-physical-contract-tests::+bracket+))
      (rewrite (file root n "comparison.sexp") record)
      (values check root))))

(defun interval (n records scale &key claimed-scale)
  "X11 interval proof over stored RECORDS; identity scale comes from the native receipt."
  (destructuring-bind (before ready pre final native) records
    (let ((g (getf ready :geometry)))
      (vttest-screen-driver:validate-screen-interval
       before ready pre final
       (list :recorder-version 3 :desired-scale scale :target-output 100
             :before-frame (getf before :frames) :before-status before :status ready
             :native-ack-output (getf native :ack-output))
       (list :area "screen-features" :pause n :backend (getf ready :backend)
             :host-id (getf ready :id) :child-pid (getf ready :child-pid)
             :window-id (getf native :window-id) :target-output 100
             :engine-geometry-generation (getf ready :engine-geometry-generation)
             :request-id (getf g :request-id) :display-epoch (getf g :display-epoch)
             :epoch (getf g :epoch) :metric-generation (getf g :metric-generation)
             :scale (or claimed-scale (getf native :scale)))))))

(defun compare (check root scale)
  (vttest-screen-native-contract::validate-comparison check root scale))
(defun comparison-refuses (label check root scale)
  (let ((c (handler-case (progn (compare check root scale) nil) (error (c) c))))
    (ok label (and (typep c '(or simple-error vttest-screen-frame:frame-refusal))
                   (not (typep c '(or program-error cell-error file-error)))))
    (push (list label (type-of c) (princ-to-string c)) *log*)))
(defun interval-refuses (label field thunk)
  (let ((c (handler-case (progn (funcall thunk) nil)
             (vttest-screen-driver:interval-refusal (c) c))))
    (ok label (and c (eq field (vttest-screen-driver:refusal-field c))))
    (push (list label (type-of c) (vttest-screen-driver:refusal-reason c)) *log*)))
(defun geometry-ok (label root n scale)
  "Record and comparison are physical; the native receipt is logical with SCALE."
  (let ((r (vttest-screen-native-contract:read-data (file root n "comparison.sexp")))
        (native (fifth (bracket root n))))
    (ok label (and (eql scale (getf r :scale)) (eql scale (getf native :scale))
                   (eql (* 640 scale) (getf r :width)) (eql (* 384 scale) (getf r :height))
                   (eql 640 (getf native :width)) (eql 384 (getf native :height))
                   (eql (* 640 384 scale scale)
                        (getf (getf r :comparison) :compared-pixels))))))

(defun pause-controls (n)
  (multiple-value-bind (check root) (x11-root n 1)
    (let ((records (bracket root n)))
      (geometry-ok (list :x11-geometry-1 n) root n 1)
      (ok (list :x11-comparison n) (listp (compare check root 1)))
      (multiple-value-bind (binding phase) (interval n records 1)
        (let ((bound (vttest-screen-driver:binding-records binding)))
          (ok (list :x11-interval n)
              (and (eq phase t) (equal (getf bound :ready) (second records))
                   (eql 1 (getf (getf bound :identity) :scale))))))))
  (let ((root (nth-value 1 (x11-root n 2))))
    (geometry-ok (list :x11-geometry-2 n) root n 2)
    (interval-refuses (list :x11-scale-2 n) :scale
                      (lambda () (interval n (bracket root n) 2)))
    (interval-refuses (list :x11-scale-2-claimed-1 n) :desired-scale
                      (lambda () (interval n (bracket root n) 2 :claimed-scale 1))))
  (let ((root (nth-value 1 (vttest-physical-contract-tests::build :screen n 1))))
    (interval-refuses (list :wayland-bracket n) :backend
                      (lambda () (interval n (bracket root n) 1))))
  (multiple-value-bind (check root) (x11-root n 1 :ack 100)
    (comparison-refuses (list :x11-native-ack n) check root 1)
    (interval-refuses (list :x11-native-ack-interval n) :native-ack-output
                      (lambda () (interval n (bracket root n) 1))))
  (multiple-value-bind (check root) (x11-root n 1 :window 124)
    (comparison-refuses (list :x11-window n) check root 1)
    (interval-refuses (list :x11-window-interval n) :window-id
                      (lambda () (interval n (bracket root n) 1)))))

(defun run-tests ()
  "Fixture roots are retained under the shared fixture *PARENT* for inspection."
  (setf *pass* 0 *log* nil
        vttest-physical-contract-tests::*parent*
        (namestring (truename (concatenate 'string (sb-posix:mkdtemp "/tmp/vtx11interval-XXXXXX") "/"))))
  (dolist (n '(13 14 15)) (pause-controls n))
  (format t "X11 screen-interval physical controls: ~d passed; roots retained in ~a~%"
          *pass* vttest-physical-contract-tests::*parent*)
  (values *pass* (reverse *log*)))
