;;;; Bounded recorder data. Uses the area's pinned no-follow raw I/O.
(in-package #:vttest-keyboard-driver)

(defun present-p (path)
  (handler-case
      (let ((prefix (if (char= (char path 0) #\/) "" ".")))
        (dolist (part (vttest-area::safe-components path))
          (setf prefix (concatenate 'string prefix "/" part))
          (need (not (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat prefix))))
                :keyboard-symlink))
        t)
    (sb-posix:syscall-error (e)
      (if (= (sb-posix:syscall-errno e) sb-posix:enoent) nil (error e)))))

(defun finite-data (form)
  (let ((nodes 0))
    (labels ((walk (x depth)
               (need (and (< depth 64) (<= (incf nodes) 100000)) :keyboard-data-limit)
               (cond ((consp x) (vttest-area::proper x 4096)
                      (dolist (y x) (walk y (1+ depth))))
                     (t (need (or (keywordp x) (null x) (eq x t)
                                  (stringp x) (rationalp x)) :keyboard-data-atom)))))
      (walk form 0)))
  form)

(defun recorder-data (path &optional many)
  "Bounded pristine forms; raw-preflight also bounds depth before allocation."
  (let* ((text (vttest-cursor-driver::text path))
         (*read-eval* nil) (*readtable* (copy-readtable nil))
         (*package* (find-package :vttest-keyboard-driver)))
    ;; Recorder monotonic times are exact ratios. Only the lexical preflight
    ;; sees substituted slashes; the reader and comparator receive original data.
    (vttest-cursor-driver::raw-preflight (substitute #\. #\/ text))
    (with-input-from-string (s text)
      (let ((eof (gensym)) (forms nil))
        (loop for f = (read s nil eof) until (eq f eof) do
          (need (< (length forms) (if many 4096 1)) :keyboard-form-count)
          (push (finite-data f) forms))
        (if many (nreverse forms)
            (progn (need forms :keyboard-empty-data) (first forms)))))))

(defun events-of (label dir)
  "NIL when absent, :MALFORMED for unsafe files, syntax or event types."
  (handler-case
      (let ((path (artifact dir label ".events.sexp")))
        (when (present-p path)
          (let ((events (recorder-data path t)))
            (dolist (e events)
              (need (evenp (vttest-area::proper e 128)) :keyboard-event-plist)
              (let ((seen nil))
                (loop for k in e by #'cddr do
                  (need (and (keywordp k) (not (member k seen))) :keyboard-event-key)
                  (push k seen)))
              (need (member (getf e :kind)
                            '(:phase :wire-out :wire-in :enqueue :driver :key-begin
                              :key-end :capture :boundary :exit)) :keyboard-event-kind)
              (dolist (k '(:seq :out :in))
                (need (typep (getf e k) '(integer 0)) :keyboard-event-offset))
              (need (and (rationalp (getf e :time)) (>= (getf e :time) 0)) :keyboard-event-time)
              (need (or (null (getf e :phase)) (stringp (getf e :phase))) :keyboard-event-phase)
              (case (getf e :kind)
                ((:wire-out :wire-in :enqueue :driver)
                 (need (stringp (getf e :bytes)) :keyboard-event-bytes))
                ((:key-begin :key-end)
                 (need (stringp (getf e :key)) :keyboard-event-key))
                (:capture (need (text-p (getf e :name)) :keyboard-event-name)))
              (when (member (getf e :kind) '(:wire-out :wire-in))
                (need (typep (getf e :end) '(integer 0)) :keyboard-event-end)))
            events)))
    (error () :malformed)))

(defun safe-image-p (path)
  ;; Inventory is not pixel proof. Do not decode image contents here.
  (when (present-p path)
    (let ((fd (vttest-area::open-input-fd path))) (sb-posix:close fd) t)))

(defun capture-id (label name)
  (when (and (stringp name) (vttest-check::capture-name-owned-p name label))
    (subseq name (+ (position #\- name) (length label) 2))))

(defun case-identity (label dir)
  "Correct ordered partial inventory is unverified; wrong evidence fails."
  (handler-case
      (let* ((events (events-of label dir))
             (captured (unless (eq events :malformed)
                         (loop for e in events when (eq (getf e :kind) :capture)
                               collect (cons (capture-id label (getf e :name)) (getf e :name)))))
             (ids (mapcar #'car captured))
             (images (mapcar (lambda (c) (safe-image-p (artifact dir (cdr c) ".png"))) captured)))
        (values (cond ((eq events :malformed) :fail)
                      ((not (prefix-p ids (capture-ids label))) :fail)
                      ((< (length ids) (length (capture-ids label))) :unverified)
                      ((every #'identity images) :pass)
                      (t :unverified)) captured))
    (error () (values :fail nil))))

(defun case-bytes (label dir)
  "Run the actual comparator; missing or unfinished evidence cannot pass."
  (handler-case
      (let* ((events (events-of label dir))
             (paths (mapcar (lambda (s) (artifact dir label s))
                            '(".transcript.raw" ".input.raw" ".result.sexp" ".exit"))))
        (when (eq events :malformed)
          (return-from case-bytes (values :fail '(:malformed-events))))
        (let* ((result (and (present-p (third paths)) (recorder-data (third paths))))
               (exit (and (present-p (fourth paths)) (recorder-data (fourth paths))))
               (output (when (present-p (first paths))
                         (map 'string #'code-char (vttest-cursor-driver::bytes (first paths)))))
               (input (when (present-p (second paths))
                        (map 'string #'code-char (vttest-cursor-driver::bytes (second paths))))))
          (when result (recorder-result result))
          (need (or (null exit) (typep exit '(integer 0 255))) :keyboard-exit)
          (when (or (getf result :failure) (getf result :errors)
                    (let ((n (getf (getf result :status) :error-count)))
                      (and n (not (eql n 0))))
                    (and exit (not (member exit '(0 3)))))
            (return-from case-bytes (values :fail '(:explicit-recorder-failure))))
          (unless (and events output input)
            ;; A missing raw wire cannot hide a contradiction in observed intent.
            ;; Ignore the second replay value: pending replies only limit
            ;; completeness suppression, not proof of a contradiction.
            (unless (action-prefix-p label events)
              (return-from case-bytes (values :fail '(:contradictory-action-prefix))))
            (return-from case-bytes (values :unverified '(:missing-byte-evidence))))
          (let* ((complete (recorder-result-complete-p result exit))
                 (errors (append (vttest-check:check-events label output input events)
                                 (vttest-check::phase-inventory-errors label events))))
            (unless complete (setf errors (partial-errors label events errors)))
            (cond (errors (values :fail errors))
                  ((not complete) (values :unverified '(:incomplete-recorder-case)))
                  (t (values :pass nil))))))
    (error (e) (values :fail (list (princ-to-string e))))))
