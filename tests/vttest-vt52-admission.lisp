;;;; Original-byte VT52 admission.
(load (merge-pathnames "vttest-vt52-sha256.lisp" *load-truename*))
(defpackage #:vttest-vt52-admission
  (:use #:cl)
  (:export #:admit-vt52-capture #:admission-p #:admission-backend #:admission-pause
   #:admission-target #:admission-surface-id #:admission-ack-output
   #:admission-window-id #:admission-native-window-id #:admission-width
   #:admission-height #:admission-scale #:admission-before #:admission-ready
   #:admission-pre #:admission-final #:admission-native #:admission-hashes
   #:admission-cursor-before #:admission-cursor-after #:admission-limitations))
(in-package #:vttest-vt52-admission)
(defmacro define-admission (&rest slots)
  `(defstruct (admission (:constructor %make-admission) (:copier nil))
     ,@(mapcar (lambda (s) `(,s nil :read-only t)) slots)))
(define-admission backend pause target surface-id ack-output window-id native-window-id
  width height scale before ready pre final native hashes cursor-before cursor-after limitations)
(defun refuse (reason) (throw 'refusal (list nil (list reason))))
(defun need (test reason) (unless test (refuse reason)))
(defun finite-tree (form)
  (let ((seen (make-hash-table :test 'eq)) (todo (list form)) (n 0))
    (loop while todo for x = (pop todo) when (consp x) do
      (need (and (not (gethash x seen)) (<= (incf n) 4096)) :tree)
      (setf (gethash x seen) t) (push (car x) todo) (push (cdr x) todo)))
  form)
(defun plist! (x)
  (let ((keys nil))
    (loop for tail = x then (cddr tail) while tail do
      (need (and (consp tail) (consp (cdr tail)) (keywordp (car tail))
       (not (member (car tail) keys))) :plist)
      (push (car tail) keys)))
  x)
(defun field (p k)
  (loop for (key value) on p by #'cddr when (eq key k) do (return-from field value))
  (refuse (list :missing-field k)))
(defun positive (x) (and (integerp x) (> x 0)))
(defun natural (x) (and (integerp x) (>= x 0)))
(defun decode (bytes) (sb-ext:octets-to-string bytes :external-format :utf-8))
(defun parse-record (bytes)
  (let ((s (decode bytes)) (quoted nil) (escape nil))
    (loop for c across s do
      (cond (escape (setf escape nil))
       ((and quoted (char= c #\\)) (setf escape t))
       ((and (not quoted) (find c '(#\\ #\| #\;))) (refuse :reader-escape))
       ((char= c #\") (setf quoted (not quoted)))
       ((and (not quoted) (char= c #\#)) (refuse :dispatch))))
    (with-standard-io-syntax
      (let ((*read-eval* nil) (*readtable* (copy-readtable nil)))
       (multiple-value-bind (form end) (read-from-string s nil :eof)
       (need (and (not (eq form :eof))
       (every (lambda (c) (find c '(#\Space #\Tab #\Newline #\Return #\Page)))
       (subseq s end))) :single-form)
       (finite-tree form))))))
(defun parse-wire (bytes)
  (let* ((s (decode bytes)) (end (1- (length s))) (start 0) (nums nil))
    (need (and (>= end 0) (char= (char s end) #\Newline)) :request-wire)
    (loop for stop = (or (position #\Space s :start start :end end) end) do
      (push (parse-integer s :start start :end stop) nums)
      (when (= stop end) (return)) (setf start (1+ stop)))
    (setf nums (nreverse nums))
    (need (and (= (length nums) 7)
       (string= s (format nil "~{~d~^ ~}~%" nums))) :request-wire)
    nums))
(defun same-fields (p keys values reason)
  (loop for key in keys for value in values do
    (need (equal (field p key) value) (list reason key))))
(defun native-binding (nr n backend wire surface ack)
  (destructuring-bind (pause window width height scale target output) wire
    (declare (ignore target output))
    (loop for p in (list nr n) for count from 0 do
      (same-fields p '(:version :backend :pause :owned :capture-exit :width :height
       :scale :surface-id :ack-output :ack-basis :capture-count :custody :png)
       (list 3 backend pause t 0 width height scale surface ack :recorder-request-binding
       count (= count 1) (format nil "pause-~2,'0d-actual.png" pause)) :native-field)
      (need (and (positive (field p :window-id))
       (eql (field p :native-window-id) (field p :window-id))
       (or (eq backend :wayland) (eql (field p :window-id) window))) :native-window))
    (need (eql (field nr :native-window-id) (field n :native-window-id)) :native-node)))
(defun trace-binding (trace hashes)
  (need (loop for tail = trace then (cdr tail) while (consp tail) count t into n
       finally (return (and (null tail) (= n 6)))) :trace-length)
  (loop for event in trace for seq from 1
       for kind in '(:pre-written :cursor-sample :authorize :done :final-written :cursor-sample)
       do (same-fields (plist! event) '(:seq :kind) (list seq kind) :trace-order))
  (loop for index in '(0 1 2 2 2 3 4 5)
       for key in '(:sha256 :linked-sha256 :request-sha256 :request-sexp-sha256
       :native-ready-sha256 :native-sha256 :sha256 :linked-sha256)
       for file in '(:pre :pre :request :request-sexp :native-ready :native :final :final)
       do (need (equal (field (nth index trace) key) (getf hashes file)) (list :trace-hash key)))
  (loop for event in (list (second trace) (sixth trace))
       for when in '(:before-authorization :after-done) do
       (same-fields event '(:when :row :col :visible) (list when 12 14 t) :cursor)))
(defun build-admission (pause raw)
  (need (eql pause 2) :pause)
  (let ((hashes nil) (forms nil))
    (loop for (key bytes) on raw by #'cddr do
      (need (typep bytes '(vector (unsigned-byte 8))) (list :missing key))
      (need (<= (length bytes) 8192) (list :too-large key))
      (setf (getf hashes key) (vttest-vt52-sha256:sha256-hex bytes))
      (unless (eq key :request) (setf (getf forms key) (parse-record bytes))))
    (dolist (key '(:before :ready :pre :final :request-sexp :native-ready :native))
      (plist! (getf forms key)))
    (let* ((b (getf forms :before)) (r (getf forms :ready))
       (p (getf forms :pre)) (f (getf forms :final))
       (q (getf forms :request-sexp)) (nr (getf forms :native-ready))
       (n (getf forms :native)) (trace (getf forms :control))
       (wire (parse-wire (getf raw :request))) (backend (field q :backend))
       (target (field q :target)) (surface (field q :surface-id)) (ack (field q :ack-output)))
      (destructuring-bind (wp window width height scale wt wa) wire
       (need (and (= wp pause) (positive window) (= width 640) (= height 384)
       (member scale '(1 2)) (natural wt)) :request-wire)
       (need (member backend '(:x11 :wayland)) :backend)
       (same-fields q '(:pause :target :ack-output) (list wp wt wa) :request-wire-mismatch)
       (need (stringp (field q :executable)) :executable)
       (need (equal (field q :status) r) :request-status)
       (same-fields r '(:backend :bytes-received) (list backend target) :status)
       (let ((g (plist! (field r :geometry))))
       (same-fields g '(:actual-width :actual-height) (list width height) :geometry)
       (when (eq backend :x11) (need (eql (field g :window-id) window) :window)))
       (need (if (eq backend :x11) (and (null surface) (= ack -1))
       (and (positive surface) (= surface window) (>= ack target))) :surface-ack)
       (native-binding nr n backend wire surface ack)
       (trace-binding trace hashes)
       (%make-admission :backend backend :pause pause :target target
       :surface-id surface :ack-output ack :window-id window
       :native-window-id (field n :native-window-id) :width width :height height :scale scale
       :before b :ready r :pre p :final f :native n :hashes hashes
       :cursor-before (second trace) :cursor-after (sixth trace)
       :limitations '(:recorder-trace-trusted :protocol-surface-ack-request-bound
       :native-ownership-harness-asserted :source-manifest-conditional
       :owned-capture-presentation-conditional))))))
(defun admit-vt52-capture (&key pause before ready request request-sexp native-ready
       pre final native control)
  (handler-case
      (values-list (catch 'refusal
       (list (build-admission pause
       (list :before before :ready ready :request request :request-sexp request-sexp
       :native-ready native-ready :pre pre :final final :native native :control control))
       nil)))
    (error () (values nil '(:malformed)))))
