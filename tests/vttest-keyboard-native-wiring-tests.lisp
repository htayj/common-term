;;;; Native wiring controls: NATIVE-MAIN, GUARDED-NATIVE-EVIDENCE, PROOF-OF,
;;;; PIXELS-OF and PIXEL-STATUS over logged stubs that always restore. Never a real
;;;; launch, process, display, PNG, font or decoder. Load after
;;;; vttest-keyboard-native-main.lisp and vttest-keyboard-context-tests.lisp.
;;;; Entry point: (NATIVE-WIRING-CONTROLS).
(in-package :vttest-keyboard-driver-tests)
(load (merge-pathnames "vttest-keyboard-native-wiring-fixtures.lisp" *load-truename*))

(defun wiring-context-controls ()
  "Each case's pixels get its own proof's EQ context, verdicts and the owned scratch."
  (multiple-value-bind (exit receipt obs log contexts root) (wiring-run)
    (check (and (= 2 (length (wiring-calls 'k::case-pixels log)))
                (every (lambda (args)
                         (destructuring-bind (label ctx prereq timing &key scratch) args
                           (and (eq ctx (cdr (assoc label contexts :test #'equal)))
                                (eq prereq :pass) (eq timing :pass)
                                (equal scratch (concatenate 'string root "pixel-scratch/")))))
                       (wiring-calls 'k::case-pixels log)))
           :wiring-same-context-into-pixels)
    (check (and (eql exit 0) (eq :pass (getf receipt :status))
                (= 26 (length (getf receipt :checks)))
                (every (lambda (k) (and (eq :pass (getf k :pixels)) (eq :pass (getf k :timing))))
                       (getf receipt :checks))
                (eq :pass (getf obs :pixels)) (eq :pass (getf obs :timing))
                (eq :pass (getf obs :cleanup)) (null (getf obs :errors)))
           :wiring-linked-pass-exit-0)))

(defun wiring-font-controls ()
  "The fourth launch value makes one closure shared by both cases; three values none."
  (multiple-value-bind (exit receipt obs log) (wiring-run)
    (declare (ignore exit receipt obs))
    (let ((made (wiring-calls 'k::make-font-proof log))
          (closures (mapcar #'fourth (wiring-calls 'k::case-proof log))))
      (check (and (= 1 (length made)) (eq *wiring-transport* (first (first made)))
                  (= 2 (length closures)) (functionp (first closures))
                  (eq (first closures) (second closures)))
             :wiring-one-lazy-font-closure)))
  (multiple-value-bind (exit receipt obs log) (wiring-run :launch :three)
    (check (and (eql exit 3) (eq :incomplete (getf receipt :status))
                (null (wiring-calls 'k::make-font-proof log))
                (null (wiring-calls :font-call log))
                (notany (lambda (k) (eq :pass (getf k :pixels))) (getf receipt :checks))
                (not (eq :pass (getf obs :pixels))))
           :wiring-three-value-launch-no-pass)))

(defun wiring-missing-controls ()
  "An absent proof, pixel or font consumer leaves the run incomplete, never PASS."
  (loop for (symbol key) in '((k::case-proof :proof) (k::case-pixels :pixels)
                              (k::make-font-proof :font))
        do (multiple-value-bind (exit receipt)
               (let ((key key)) (wiring-without symbol (lambda () (wiring-run key :missing))))
             (check (and (eql exit 3) (eq :incomplete (getf receipt :status))
                         (notany (lambda (k) (eq :pass (getf k :pixels))) (getf receipt :checks)))
                    (list :wiring-missing-consumer key)))))

(defun wiring-failure-controls ()
  (let ((*wiring-fail* (wiring-name "keyboard-keypad" "pause01")))
    (multiple-value-bind (exit receipt obs) (wiring-run)
      (check (and (eql exit 1) (eq :fail (getf receipt :status)) (eq :fail (getf obs :pixels))
                  (eq :fail (getf (find *wiring-fail* (getf receipt :checks)
                                        :key (lambda (k) (getf k :name)) :test #'equal)
                                  :pixels)))
             :wiring-required-pixel-fail-exit-1)))
  (multiple-value-bind (exit receipt) (wiring-run :cleanup :unverified)
    (check (and (eql exit 3) (eq :incomplete (getf receipt :status))) :wiring-cleanup-gates-pass))
  (multiple-value-bind (checks failed evidence errors)
      (call-with-stubs (list (cons 'k::native-evidence #'wiring-signal))
                       (lambda () (k::guarded-native-evidence "/fixture/" nil nil)))
    (check (and (null checks) (eq t failed) (null evidence)
                (equal errors '((:native-evidence-signal simple-error))))
           :wiring-guarded-evidence-signal))
  (multiple-value-bind (exit receipt obs)
      (call-with-stubs (list (cons 'k::native-evidence #'wiring-signal)) #'wiring-run)
    (check (and (eql exit 1) (eq :fail (getf receipt :status))
                (member '(:native-evidence-signal simple-error) (getf obs :errors) :test #'equal))
           :wiring-evidence-signal-receipt-exit-1)))

(defun wiring-helper-controls ()
  (multiple-value-bind (timing reasons prereq)
      (call-with-stubs (list (cons 'k::case-proof (wiring-returning :maybe nil :pass nil nil)))
                       (lambda () (k::proof-of "keyboard-cursor" "/fixture/" :pass nil)))
    (check (and (eq :fail timing) (eq :pass prereq) (equal reasons '((:case-proof-state keyword))))
           :wiring-unknown-proof-state-fails))
  (check (eq :fail (k::pixels-of "keyboard-cursor" nil :pass :pass "/fixture/"))
         :wiring-passing-proof-without-context-fails)
  (check (eq :unverified (k::pixels-of "keyboard-cursor" nil :unverified :pass "/fixture/"))
         :wiring-no-context-unverified))

(defun wiring-summary-controls ()
  "Summary PASS needs identity PASS, the original name and linked timing."
  (let* ((name "01-keyboard-cursor-keys0-up")
         (check (list :name name :pixels :pass :timing :pass)))
    (check (eq :pass (k::pixel-status :pass check name :pass)) :wiring-summary-linked-pass)
    (loop for (tag . args) in `((:identity :unverified ,check ,name :pass)
                                (:timing :pass ,check ,name :unverified)
                                (:no-timing :pass ,check ,name)
                                (:no-original :pass ,check nil :pass)
                                (:no-check :pass nil ,name :pass))
          do (check (eq :unverified (apply #'k::pixel-status args))
                    (list :wiring-summary-unverified tag)))
    (check (eq :fail (k::pixel-status :pass check "02-other" :pass)) :wiring-summary-wrong-name)
    (check (eq :fail (k::pixel-status :fail check name :pass)) :wiring-summary-identity-fail)))

(defun wiring-source-controls ()
  (loop for text in (mapcar #'wiring-text *wiring-sources*)
        for pins in '(("(launch-keyboard-gate backend root request)"
                       "(make-font-proof transport root)"
                       "(guarded-native-evidence root font-proof scratch)"
                       "errors (append errors x)")
                      ("(case-proof label dir bytes font-proof)"
                       "(pixels-of label ctx prereq timing scratch)"
                       "(case-pixels label ctx prereq timing :scratch scratch)"
                       ":native-evidence-signal")
                      ("(defun pixel-status (identity check original &optional timing)"
                       "((null original) :unverified)"))
        do (dolist (pin pins) (check (search pin text) (list :wiring-source-pin pin)))))

(defun native-wiring-controls ()
  "Stubbed native wiring controls only; never a real native run."
  (let ((originals (mapcar #'fdefinition *wiring-stubbed*)))
    (wiring-context-controls)
    (wiring-font-controls)
    (wiring-missing-controls)
    (wiring-failure-controls)
    (wiring-helper-controls)
    (wiring-summary-controls)
    (wiring-source-controls)
    (check (every #'eq originals (mapcar #'fdefinition *wiring-stubbed*))
           :wiring-stubs-restored)))
