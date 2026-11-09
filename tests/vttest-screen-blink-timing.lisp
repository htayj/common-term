;;;; Pure validation of the renderer's blink receipts in status plists.
;;;; The proof covers exactly the renderer's existing half-second minimum
;;;; between toggle DECISIONS, timed on SBCL's monotonic internal real time.
;;;; It makes no presentation, scanout or compositor-timing claim and sets no
;;;; upper bound: lateness is reported as data only.
(defpackage #:vttest-screen-blink-timing
  (:use #:cl)
  (:export #:*toggle-decision-minimum* #:*receipt-keys* #:blink-receipt-errors
           #:validate-blink-chain #:blink-capture-ready-p))
(in-package #:vttest-screen-blink-timing)

(defparameter *toggle-decision-minimum* 1/2
  "The renderer's existing minimum between blink toggle decisions, in seconds.")
(defparameter *receipt-keys*
  '(:frames :bytes-received :blink-phase :blink-visible :blink-epoch :blink-epoch-at
    :blink-toggles :blink-toggled-at :blink-previous-at :blink-toggle-frame))

(defun natural-p (x) (typep x '(integer 0)))
(defun time-p (x) "An exact nonnegative rational number of seconds." (typep x '(rational 0)))
(defun boolean-p (x) (or (eq x t) (eq x nil)))
(defun proper-list-p (x) (and (listp x) (handler-case (list-length x) (error () nil))))
(defun plist-p (x) (and (proper-list-p x) (evenp (length x))))

(defun same-values-p (statuses keys)
  (every (lambda (key)
           (every (lambda (status) (equal (getf status key) (getf (first statuses) key)))
                  statuses))
         keys))

(defun toggle-in-epoch-p (s)
  "True when S's most recent lifetime toggle was decided inside its current epoch."
  (let ((e (getf s :blink-epoch-at)))
    (and e (plusp (getf s :blink-toggles)) (> (getf s :blink-toggled-at) e)
         (>= (getf s :blink-previous-at) e))))

(defun blink-receipt-errors (status)
  "Shape errors of one status plist, or NIL when every receipt field is well formed."
  (unless (plist-p status) (return-from blink-receipt-errors '(:not-a-plist)))
  (let ((errors '()))
    (flet ((need (ok what) (unless ok (push what errors)))
           (field (key) (getf status key)))
      (dolist (key *receipt-keys*)
        (need (not (eq (getf status key '%missing) '%missing)) (list :missing key)))
      (need (field :id) :missing-id)
      (dolist (key '(:frames :bytes-received :blink-epoch :blink-toggles))
        (need (natural-p (field key)) (list :not-natural key)))
      (dolist (key '(:blink-phase :blink-visible))
        (need (boolean-p (field key)) (list :not-boolean key)))
      (need (if (eql 0 (field :blink-epoch))
                (null (field :blink-epoch-at))
                (time-p (field :blink-epoch-at)))
            :epoch-at)
      (if (eql 0 (field :blink-toggles))
          (need (notany #'field '(:blink-toggled-at :blink-previous-at :blink-toggle-frame))
                :toggle-receipt-without-toggle)
          (let ((at (field :blink-toggled-at)) (previous (field :blink-previous-at))
                (frame (field :blink-toggle-frame)) (frames (field :frames)))
            (need (and (time-p at) (time-p previous) (<= previous at)) :toggle-times)
            (need (and (natural-p frame) (plusp frame) (natural-p frames) (<= frame frames))
                  :toggle-frame))))
    (nreverse errors)))

(defun generations (statuses)
  "One status per run of equal :BLINK-TOGGLES (its last), in observation order."
  (loop for (status next) on statuses
        when (or (null next) (/= (getf status :blink-toggles) (getf next :blink-toggles)))
          collect status))

(defun validate-blink-chain (statuses)
  "Validate observed statuses as one unbroken chain of directly observed toggles.
Return a plist; :VALID is T only when every check passes."
  (let ((errors '()) (intervals '()) (reset-to-first nil) (gens '()))
    (flet ((fail (&rest what) (push what errors))
           (report ()
             (return-from validate-blink-chain
               (list :valid (null errors) :errors (reverse errors)
                     :basis "toggle-decision minimum 1/2 s; not presentation timing"
                     :minimum *toggle-decision-minimum*
                     :epoch (and (plist-p (first statuses)) (getf (first statuses) :blink-epoch))
                     :toggles (mapcar (lambda (s) (getf s :blink-toggles)) gens)
                     :intervals intervals
                     :lateness (mapcar (lambda (i) (- i *toggle-decision-minimum*)) intervals)
                     :reset-to-first reset-to-first))))
      (unless (and (proper-list-p statuses) statuses) (fail :no-statuses) (report))
      (loop for status in statuses for index from 0
            for shape = (blink-receipt-errors status)
            when shape do (fail :malformed index shape))
      (when errors (report))
      (unless (same-values-p statuses '(:id :bytes-received :blink-epoch :blink-epoch-at))
        (fail :source-or-epoch-changed))
      (unless (every (lambda (s) (eq t (getf s :blink-visible))) statuses) (fail :not-visible))
      (loop for status in statuses for index from 0
            unless (toggle-in-epoch-p status) do (fail :toggle-outside-epoch index))
      (loop for (a b) on statuses while b
            unless (and (<= (getf a :frames) (getf b :frames))
                        (<= (getf a :blink-toggles) (getf b :blink-toggles)))
              do (fail :observation-order-decreased))
      (when errors (report))
      (setf gens (generations statuses))
      (dolist (status statuses)
        (unless (same-values-p (list status (find (getf status :blink-toggles) gens
                                                  :key (lambda (g) (getf g :blink-toggles))))
                               '(:blink-phase :blink-toggled-at :blink-previous-at
                                 :blink-toggle-frame))
          (fail :generation-inconsistent (getf status :blink-toggles))))
      (let ((first (first gens)))
        (when (= (getf first :blink-previous-at) (getf first :blink-epoch-at))
          (setf reset-to-first (- (getf first :blink-toggled-at) (getf first :blink-previous-at)))
          (when (< reset-to-first *toggle-decision-minimum*) (fail :reset-to-first-below-minimum))))
      (loop for (a b) on gens while b
            for count = (getf b :blink-toggles)
            for interval = (- (getf b :blink-toggled-at) (getf b :blink-previous-at))
            do (push interval intervals)
               (unless (= count (1+ (getf a :blink-toggles))) (fail :toggle-gap count))
               (unless (= (getf b :blink-previous-at) (getf a :blink-toggled-at))
                 (fail :previous-at-mismatch count))
               (when (eq (getf a :blink-phase) (getf b :blink-phase)) (fail :phase-repeated count))
               (unless (> (getf b :blink-toggle-frame) (getf a :blink-toggle-frame))
                 (fail :toggle-frame-not-increasing count))
               (when (< interval *toggle-decision-minimum*) (fail :interval-below-minimum count)))
      (setf intervals (nreverse intervals))
      (when (< (length gens) 4) (fail :fewer-than-four-toggle-generations (length gens)))
      (report))))

(defun blink-capture-ready-p (ready pre final &key (phase nil phase-p))
  "True when READY, PRE and FINAL carry one identical frame, source and blink
receipt, so no frame or toggle decision happened during the capture, and READY's
most recent lifetime toggle, if any, lies in its current epoch.
The second value lists the refusals."
  (let ((errors '()) (statuses (list ready pre final)))
    (loop for status in statuses for name in '(:ready :pre :final)
          for shape = (blink-receipt-errors status)
          when shape do (push (list :malformed name shape) errors))
    (unless errors
      (unless (same-values-p statuses (cons :id *receipt-keys*))
        (push :changed-during-capture errors))
      (unless (eq t (getf ready :blink-visible)) (push :not-visible errors))
      (unless (or (zerop (getf ready :blink-toggles)) (toggle-in-epoch-p ready))
        (push :toggle-outside-epoch errors))
      (when (and phase-p (not (and (boolean-p phase) (eq phase (getf ready :blink-phase)))))
        (push :wrong-phase errors)))
    (values (null errors) (nreverse errors))))
