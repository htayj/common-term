;;;; Native blink helper tests. Standalone: sbcl --non-interactive --load this file.
;;;; Scripted statuses, a counting clock and a no-op sleep: no host, no waiting.
;;;; Pure selection replay fixtures live in vttest-screen-blink-selection-tests.lisp.
(load (merge-pathnames "vttest-screen-blink-native.lisp" *load-truename*))
(defpackage #:vttest-screen-blink-native-tests
  (:use #:cl #:vttest-screen-blink-native #:vttest-screen-blink-timing))
(in-package #:vttest-screen-blink-native-tests)

(defvar *checks* 0)
(defun check (name value)
  (incf *checks*)
  (format t "~S~%" (list :check name :pass (not (null value))))
  (unless value (error "Blink native check failed: ~S" name)))

(defun gen (g &key (step 1/2) (epoch-at 10) (previous nil))
  "Status of lifetime toggle G, STEP apart, toggle 0 at EPOCH-AT."
  (list :id 1 :frames (+ 10 g) :bytes-received 500 :blink-phase (evenp g)
        :blink-visible t :blink-epoch 3 :blink-epoch-at epoch-at :blink-toggles g
        :blink-toggled-at (+ 10 (* g step))
        :blink-previous-at (or previous (+ 10 (* (1- g) step))) :blink-toggle-frame (+ 10 g)))
(defun gens (first count &rest keys)
  (loop for g from first below (+ first count) collect (apply #'gen g keys)))
(defun with (status &rest pairs)
  (let ((copy (copy-list status)))
    (loop for (k v) on pairs by #'cddr do (setf (getf copy k) v))
    copy))

(defvar *polls* 0)
(defun script (statuses)
  "Status thunk returning STATUSES in order, then repeating the last."
  (setf *polls* 0)
  (lambda () (incf *polls*) (if (rest statuses) (pop statuses) (first statuses))))
(defun counting-clock () (let ((now 0)) (lambda () (incf now))))
(defun nop ())

(defun select (statuses wanted)
  (select-fresh-phase (script statuses) wanted 500 :seconds 20
                      :clock-fn (counting-clock) :sleep-fn #'nop))
(defun collect (statuses &optional seed)
  (collect-blink-chain (script statuses) :seed seed :seconds 20
                       :clock-fn (counting-clock) :sleep-fn #'nop))
(defun refusal (thunk)
  "The BLINK-REFUSAL THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil) (blink-refusal (c) c)))
(defun reason (thunk) (let ((c (refusal thunk))) (and c (blink-refusal-reason c))))
(defun chain-error-p (thunk error)
  (let ((r (reason thunk))) (and (listp r) (assoc error r))))

(defun phase-tests ()
  (check :pause-13-hidden-required (equal '(nil t) (multiple-value-list (expected-blink-phase 13))))
  (check :pause-14-visible (equal '(t t) (multiple-value-list (expected-blink-phase 14))))
  (check :pause-15-visible (equal '(t t) (multiple-value-list (expected-blink-phase 15))))
  (check :pause-12-omitted (equal '(nil nil) (multiple-value-list (expected-blink-phase 12)))))

(defun selection-tests ()
  (multiple-value-bind (status polls) (select (gens 2 3) nil)
    (check :fresh-hidden-selected (equal status (gen 3)))
    (check :raw-polls-kept (equal polls (gens 2 2))))
  (check :fresh-visible-after-new-toggle
         (equal (gen 4) (select (list (gen 2) (gen 2) (gen 3) (gen 4)) t)))
  (let ((c (refusal (lambda () (select (list (gen 2)) t)))))
    (check :deadline (and c (eq :deadline (blink-refusal-reason c))))
    (check :deadline-polls-retained (= 20 (length (blink-refusal-observations c)) *polls*)))
  (check :changed-source-refused
         (equal '((:source-changed 1)) (reason (lambda () (select (list (gen 2) (with (gen 3) :id 2)) nil)))))
  (check :two-four-three-refused
         (equal '((:order 2)) (reason (lambda () (select (list (gen 2) (gen 4) (gen 3)) nil)))))
  (check :malformed-first (equal '((:malformed 0)) (reason (lambda () (select (list (list :id 1)) nil)))))
  (let ((stale (gen 2 :epoch-at 111/10)) (fresh (gen 3 :epoch-at 111/10 :previous 111/10)))
    (check :repeated-stale-prefix-then-fresh (equal fresh (select (list stale stale stale fresh) nil)))
    (check :stale-toggle-refused
           (equal '((:stale-toggle 1)) (reason (lambda () (select (gens 2 2 :epoch-at 111/10) nil)))))))

(defun collector-tests ()
  (multiple-value-bind (statuses report) (collect (gens 1 6))
    (check :stops-at-four (and (equal statuses (gens 1 4)) (= 4 *polls*)))
    (check :four-toggles-reported (equal '(1 2 3 4) (getf report :toggles))))
  (multiple-value-bind (statuses report) (collect (gens 3 5) (gen 2))
    (check :seeded-first (and (equal statuses (gens 2 4)) (= 3 *polls*)))
    (check :seeded-report (equal '(2 3 4 5) (getf report :toggles))))
  (check :three-deadline (eq :deadline (reason (lambda () (collect (gens 1 3))))))
  (check :gap-immediate (and (chain-error-p (lambda () (collect (list (gen 1) (gen 2) (gen 4) (gen 5))))
                                            :toggle-gap)
                             (= 3 *polls*)))
  (check :below-minimum-immediate
         (and (chain-error-p (lambda () (collect (gens 2 4 :step 499/1000))) :interval-below-minimum)
              (= 2 *polls*)))
  (check :half-accepted (equal '(1/2 1/2 1/2) (getf (nth-value 1 (collect (gens 2 4))) :intervals)))
  (check :late-accepted (equal '(7/4 7/4 7/4) (getf (nth-value 1 (collect (gens 2 4 :step 7/4))) :intervals)))
  (let ((c (refusal (lambda () (collect (gens 2 4 :epoch-at 111/10))))))
    (check :stale-first-refused-not-dropped
           (and c (assoc :toggle-outside-epoch (blink-refusal-reason c))
                (equal (blink-refusal-observations c) (gens 2 1 :epoch-at 111/10))))))

(defun proof-tests ()
  (let* ((chain (blink-chain-record 15 (gens 1 2) (gens 2 4)))
         (selection (blink-selection-record 15 t 500 (gens 5 2)))
         (ready (with (gen 6) :frames 17)))
    (flet ((refused (error &key (chain chain) (selection selection) (ready ready) (target 500)
                              (pause 15))
             (multiple-value-bind (ok errors) (validate-blink-proof pause chain selection ready target)
               (and (not ok) (member error errors :test #'equal)))))
      (check :exact-keys (equal *chain-record-keys* (loop for k in chain by #'cddr collect k)))
      (check :proof-accepted (validate-blink-proof 15 chain selection ready 500))
      (check :tampered-report (refused :report-not-recomputed
                                       :chain (with chain :report (with (getf chain :report) :lateness nil))))
      (check :stored-pass-not-trusted (refused :chain-invalid :chain (with chain :statuses (gens 2 3))))
      (check :statuses-42 (refused :chain-invalid :chain (with chain :statuses 42)))
      (check :missing-key (refused :record-keys :chain (butlast chain 2)))
      (check :version-1-refused (refused :version :chain (with chain :version 1)))
      (check :chain-first-not-s0 (refused :chain-first-not-s0
                                          :chain (blink-chain-record 15 (gens 2 2) (gens 2 4))))
      (check :chain-after-s1-start (refused :chain-not-before-s1
                                            :selection (blink-selection-record 15 t 500 (gens 4 3))))
      (check :s0-appended (refused '(:s0 (:appended 1))
                                   :chain (blink-chain-record 15 (gens 1 3) (gens 2 4))))
      (check :s1-wrong-phase (refused '(:s1 :phase-mismatch)
                                      :selection (blink-selection-record 15 nil 500 (gens 4 2))))
      (check :toggle-before-ready (refused '(:s1 :ready-not-linked) :ready (gen 7)))
      (check :id-mismatch (refused '(:ready-mismatch :id) :ready (with ready :id 2)))
      (check :epoch-at-mismatch (refused '(:ready-mismatch :blink-epoch-at)
                                         :ready (with ready :blink-epoch-at 21/2)))
      (check :target-mismatch (refused :target-mismatch :target 501))
      (check :wrong-pause (refused :not-chain-pause :pause 14)))))

(phase-tests)
(selection-tests)
(collector-tests)
(proof-tests)
(format t "~S~%" (list :blink-native-tests :passed *checks*))
