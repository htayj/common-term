;;;; Prove unfinished action intent and its enqueue/accepted FIFO prefix.
(in-package #:vttest-keyboard-driver)

(defun action-prefix-p (label events)
  ;; This supplements, never replaces, CHECK-EVENTS' accepted-wire checks.
  (let* ((spec (case-of label)) (path (second spec))
         (actions
           (append
            (loop for n in path for p from 0 collect
              (list :driver (format nil "navigate-~d" p) nil (format nil "~d~c" n #\Return)))
            (loop for p below (third spec) append
              (loop for k in (append (fourth spec) '("Tab"))
                    for b in (append (vttest-check::expected-key-byte-list label p)
                                     (list (string #\Tab)))
                    collect (list :key-begin (format nil "keys-~d" p) k b)))
            (list (list :driver "pause-1" nil (string #\Return))
                  (list :driver "return-main" nil (format nil "0~c" #\Return))
                  (list :driver "exit" nil (format nil "0~c" #\Return)))))
         (active nil) (queued 0) (accepted 0) (fifo nil))
    (block replay
      (dolist (e events)
        (let ((kind (getf e :kind)) (bytes (getf e :bytes)))
          (case kind
            ((:driver :key-begin)
             (when (or active fifo (null actions)) (return-from replay nil))
             (setf active (pop actions) queued 0 accepted 0)
             (unless (and (eq kind (first active))
                          (equal (getf e :phase) (second active))
                          (if (eq kind :driver) (equal bytes (fourth active))
                              (equal (getf e :key) (third active))))
               (return-from replay nil)))
            (:enqueue
             (unless (and (member (getf e :source) '(:input :reply))
                          (plusp (length bytes))) (return-from replay nil))
             (when (eq (getf e :source) :input)
               ;; Validate intent now, even if none of these bytes was written.
               (unless (and active (equal (getf e :phase) (second active))
                            (<= (+ queued (length bytes)) (length (fourth active)))
                            (string= bytes (fourth active) :start2 queued
                                     :end2 (+ queued (length bytes))))
                 (return-from replay nil))
               (incf queued (length bytes)))
             (setf fifo (append fifo (list (list (getf e :source) bytes)))))
            (:wire-in
             (let ((head (first fifo)) (n (length bytes)))
               (unless (and head (eq (first head) (getf e :source))
                            (<= n (length (second head)))
                            (string= bytes (second head) :end2 n))
                 (return-from replay nil))
               (setf (second head) (subseq (second head) n))
               (when (zerop (length (second head))) (pop fifo))
               (when (eq (getf e :source) :input)
                 (unless (and active (equal (getf e :phase) (second active)))
                   (return-from replay nil))
                 (incf accepted n)
                 (when (and (eq (first active) :driver)
                            (= accepted (length (fourth active))))
                   (setf active nil)))))
            (:key-end
             (unless (and active (eq (first active) :key-begin) (null fifo)
                          (equal (getf e :phase) (second active))
                          (equal (getf e :key) (third active))
                          (= accepted (length (fourth active))))
               (return-from replay nil))
             (setf active nil))
            (:phase (when (or active fifo) (return-from replay nil))))))
      ;; Pending terminal replies have no action intent here. Keep their strict
      ;; completeness report rather than inventing a second query oracle.
      (values t (notany (lambda (q) (eq (first q) :reply)) fifo)))))

(defun recorder-plist (value)
  (need (evenp (vttest-area::proper value)) :keyboard-recorder-plist)
  (let ((seen nil))
    (loop for k in value by #'cddr do
      (need (and (keywordp k) (not (member k seen))) :keyboard-recorder-key)
      (push k seen)))
  value)

(defun recorder-result (result)
  ;; Only consumed fields are typed. Missing fields describe an unfinished case.
  (recorder-plist result)
  (flet ((check (plist key predicate)
           (let ((missing (gensym)))
             (let ((value (getf plist key missing)))
               (unless (eq value missing)
                 (need (funcall predicate value) :keyboard-result-type))))))
    (dolist (key '(:returned-to-menu :inventory-ok))
      (check result key (lambda (v) (or (null v) (eq v t)))))
    (check result :failure (lambda (v) (or (null v) (stringp v))))
    (check result :errors (lambda (v) (vttest-area::proper v 32) t))
    (let ((status (getf result :status)))
      (recorder-plist status)
      (dolist (key '(:error-count :input-pending))
        (check status key (lambda (v) (typep v '(integer 0)))))
      (check status :process-state
             (lambda (v) (member v '(nil :running :exited :signaled :detached))))
      (check status :transport-state
             (lambda (v) (member v '(:open :hangup :waiting-for-slave))))))
  result)

(defun recorder-result-complete-p (result exit)
  ;; Nullable fields must be recorded explicitly, not supplied by GETF's default.
  (flet ((present (plist keys)
           (loop for key in keys always
             (loop for tail on plist by #'cddr thereis (eq key (car tail))))))
    (and (present result '(:returned-to-menu :inventory-ok :failure :errors :status))
         (present (getf result :status)
                  '(:error-count :process-state :transport-state :input-pending))
         (vttest-check:case-complete-p result exit))))

(defun partial-errors (label events errors)
  ;; Remove only completeness issues whose observed evidence is a valid prefix.
  (let* ((path (second (case-of label)))
         (wanted (append '("startup")
                         (loop for p below (length path) collect (format nil "navigate-~d" p))
                         (loop for p below (third (case-of label)) collect (format nil "keys-~d" p))
                         '("pause-1" "return-main" "exit")))
         (drivers (append (mapcar (lambda (n) (format nil "~d~c" n #\Return)) path)
                          (list (string #\Return) (format nil "0~c" #\Return)
                                (format nil "0~c" #\Return)))))
    (multiple-value-bind (valid pending-safe) (action-prefix-p label events)
      (setf valid (and valid pending-safe))
      (remove-if
       (lambda (e)
         (or (and valid pending-safe (equal e "Unclosed key/pending enqueue/driver input"))
             (and valid (or (equal e "Extra or missing native key inventory")
                            (search "Native key order/per-key bytes mismatch in " e)))
             (and valid (equal e "Source-defined phase inventory/order mismatch")
                  (prefix-p (loop for e in events when (eq (getf e :kind) :phase)
                                 collect (getf e :phase)) wanted))
             (and valid (equal e "Driver navigation/advance/normal-exit intent mismatch")
                  (prefix-p (loop for e in events when (eq (getf e :kind) :driver)
                                 collect (getf e :bytes)) drivers))))
       errors))))
