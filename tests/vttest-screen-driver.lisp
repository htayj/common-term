;;;; Pure offline interval admission. No loads, product objects, pixels or I/O.
(defpackage #:vttest-screen-driver
  (:use #:cl)
  (:export #:validate-screen-interval #:interval-refusal #:refusal-field
           #:refusal-reason #:binding-records))
(in-package #:vttest-screen-driver)

(define-condition interval-refusal (error)
  ((field :initarg :field :reader refusal-field)
   (reason :initarg :reason :reader refusal-reason))
  (:report (lambda (c s) (format s "Screen interval ~s: ~s"
                               (refusal-field c) (refusal-reason c)))))
(defun need (test field reason)
  (unless test (error 'interval-refusal :field field :reason reason)))

(defun bounded-copy (value field)
  "Validate and copy a finite portable tree before any plist traversal."
  (let ((nodes 0) (active (make-hash-table :test #'eq)))
    (labels ((walk (x depth)
               (need (and (< (incf nodes) 8192) (< depth 16)) field :size-limit)
               (cond ((consp x)
                      (need (not (gethash x active)) field :circular)
                      (setf (gethash x active) t)
                      (let ((result (cons (walk (car x) (1+ depth))
                                          (walk (cdr x) depth))))
                        (remhash x active) result))
                     ((stringp x) (need (<= (length x) 256) field :string-limit)
                                  (copy-seq x))
                     (t (need (or (null x) (eq x t) (keywordp x)
                                  (typep x '(integer 0 1152921504606846975))
                                  (eql x -1)) field :atom-type) x))))
      (walk value 0))))

(defparameter +status-keys+
  '(:id :state :backend :worker-alive-p :error-count :rows :columns
    :engine-geometry-generation :pty-rows :pty-columns :geometry :frames
    :blink-phase :blink-visible :bytes-received :transport-state :process-state
    :child-pid :input-pending :input-limit))
(defparameter +geometry-keys+
  '(:request-id :display-epoch :epoch :metric-generation :cell-width :cell-height
    :phase :reason :requested-columns :requested-rows :deadline :requested-width
    :requested-height :actual-width :actual-height :columns :rows :presentation
    :target-presentation :columns :rows :width :height :cell-width :cell-height
    :backend :configure-sequence :window-id :depth :bits-per-pixel))
(defparameter +barrier-keys+
  '(:recorder-version :desired-scale :target-output :before-frame :before-status
    :status :native-ack-output))
(defparameter +identity-keys+
  '(:area :pause :backend :host-id :child-pid :window-id :target-output
    :engine-geometry-generation :request-id :display-epoch :epoch
    :metric-generation :scale))

(defun schema (plist keys field &optional geometry-p)
  ;; Lists have already passed bounded-copy, including cycle detection.
  (let ((seen nil))
    (loop for tail = plist then (cddr tail) while tail do
      (need (and (consp tail) (consp (cdr tail))) field :improper-plist)
      (let ((key (car tail)))
        (need (member key keys) field :unknown-key)
        (push key seen)))
    (dolist (key (remove-duplicates keys))
      (need (= (count key seen) (count key keys)) key :cardinality)
      (when (and geometry-p (> (count key keys) 1))
        (let ((values (loop for (k v) on plist by #'cddr when (eq k key) collect v)))
          (need (every (lambda (v) (and (typep v '(integer 1))
                                       (eql v (first values)))) values)
                key :duplicate-value))))
    plist))
(defun exact (record key value)
  (need (equal (getf record key) value) key :unexpected-value))
(defun natural (record key &optional (minimum 0))
  (need (typep (getf record key) `(integer ,minimum 1152921504606846975))
        key :integer-type))

;; A closure owns the copied tree; every public observation is a fresh copy.
(defun binding-records (binding) (funcall binding))
(defun validate-screen-interval (before ready status final barrier identity)
  "Return an isolated snapshot closure and the present boolean blink phase.
Signal INTERVAL-REFUSAL with FIELD and REASON on refusal. This validates only
raw record consistency, never native custody, session freshness or X11 geometry.
IDENTITY is an expected-value descriptor, not proof of ownership. Integration
must separately prove source/launch/receipt/session and external geometry custody.
Identical forged replay is not distinguishable by record equality."
  (destructuring-bind (before ready status final barrier identity)
      (mapcar (lambda (x) (bounded-copy x :record))
              (list before ready status final barrier identity))
    (schema identity +identity-keys+ :identity)
    (schema barrier +barrier-keys+ :barrier)
    (exact identity :area "screen-features")
    (need (member (getf identity :pause) '(13 14 15)) :pause :scope)
    (exact identity :backend :x11)
    (exact identity :scale 1)
    (dolist (key '(:host-id :child-pid :window-id)) (natural identity key 1))
    (dolist (key '(:target-output :engine-geometry-generation :request-id
                   :display-epoch :epoch :metric-generation)) (natural identity key))
    (exact barrier :recorder-version 3)
    (exact barrier :desired-scale 1)
    ;; The X11 producer has no native key ACK. -1 is its untouched sentinel.
    (exact barrier :native-ack-output -1)
    (exact barrier :target-output (getf identity :target-output))
    (dolist (s (list before ready status final))
      (schema s +status-keys+ :status)
      (let ((g (schema (getf s :geometry) +geometry-keys+ :geometry t)))
        (dolist (kv '((:state :attached) (:backend :x11) (:worker-alive-p t)
                      (:error-count 0) (:rows 24) (:columns 80) (:pty-rows 24)
                      (:pty-columns 80) (:input-pending 0) (:blink-visible t)
                      (:transport-state :open) (:process-state :running)))
          (exact s (first kv) (second kv)))
        (need (member (getf s :blink-phase) '(nil t)) :blink-phase :boolean)
        (natural s :frames 1) (natural s :input-limit 1)
        (dolist (pair '((:id :host-id) (:child-pid :child-pid)
                        (:bytes-received :target-output)
                        (:engine-geometry-generation :engine-geometry-generation)))
          (exact s (first pair) (getf identity (second pair))))
        ;; X11's settled GetGeometry callback publishes this exact reason.
        (dolist (kv '((:phase :settled) (:reason :server-compatible) (:backend :x11)
                      (:columns 80) (:rows 24) (:cell-width 8) (:cell-height 16)
                      (:width 640) (:height 384) (:actual-width 640) (:actual-height 384)
                      (:requested-columns 80) (:requested-rows 24)
                      (:requested-width 640) (:requested-height 384)))
          (exact g (first kv) (second kv)))
        (dolist (key '(:request-id :display-epoch :epoch :metric-generation :window-id))
          (exact g key (getf identity key)))
        (exact g :request-id (getf s :engine-geometry-generation))
        (natural g :deadline)
        ;; X11 initializes this diagnostic slot to NIL. Readiness can follow
        ;; GetGeometry without ConfigureNotify; the sequence is not causal.
        (need (typep (getf g :configure-sequence) '(or null (unsigned-byte 16)))
              :configure-sequence :diagnostic-type)
        (dolist (key '(:presentation :target-presentation :depth :bits-per-pixel))
          (natural g key 1))
        (need (>= (getf g :presentation) (getf g :target-presentation))
              :presentation :unsettled)))
    (exact barrier :before-frame (getf before :frames))
    (exact barrier :before-status before)
    (exact barrier :status ready)
    ;; All settled geometry details remain identical across the forced redraw,
    ;; except presentation, which may advance with the completed frame.
    (let ((bg (getf before :geometry)) (rg (getf ready :geometry)))
      (dolist (key (remove :presentation (remove-duplicates +geometry-keys+)))
        (exact rg key (getf bg key)))
      (need (>= (getf rg :presentation) (getf bg :presentation))
            :presentation :regression))
    (need (> (getf ready :frames) (getf before :frames)) :frames :not-fresh)
    (need (equal ready status) :status :interval-changed)
    (need (equal ready final) :final :interval-changed)
    (let ((snapshot (list :before before :ready ready :status status :final final
                          :barrier barrier :identity identity)))
      (values (lambda ()
                (mapcar (lambda (x) (bounded-copy x :binding)) snapshot))
              (getf ready :blink-phase)))))

;;;; Load executable helpers only inside the standalone area dispatcher.
(when (find-package :vttest-area)
  (load (merge-pathnames "vttest-screen-manifest.lisp" *load-truename*)))
