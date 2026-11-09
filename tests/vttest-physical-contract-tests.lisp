;;;; Physical comparison-record controls for the screen, double-size and
;;;; character-set VALIDATE-COMPARISON contracts over rooted fixtures.
(require :sb-posix)
(dolist (f '("vttest-screen-native-contract" "vttest-double-size-native-contract"
             "vttest-charsets-native-contract" "vttest-physical-contract-test-fixtures"))
  (load (merge-pathnames (concatenate 'string f ".lisp") *load-truename*)))
(in-package :vttest-physical-contract-tests)

(defvar *pass* 0)
(defvar *log* nil "Each refusal as (LABEL CONDITION-TYPE MESSAGE).")

(defun ok (label test) (unless test (error "Control failed: ~s" label)) (incf *pass*))
(defun contract-refusal-p (c)
  "A contract NEED refusal or a frame refusal; never an arity, cell or file error."
  (and (typep c '(or simple-error vttest-screen-frame:frame-refusal
                     vttest-double-size-native-contract:frame-refusal))
       (not (typep c '(or program-error cell-error file-error)))))
(defun check-call (area check root scale)
  (if (eq scale :default)
      (funcall (area-fn area "VALIDATE-COMPARISON") check root)
      (funcall (area-fn area "VALIDATE-COMPARISON") check root scale)))
(defun accepts (label area n scale &rest options)
  (multiple-value-bind (check root) (apply #'build area n (if (eq scale :default) 1 scale) options)
    (ok label (listp (check-call area check root scale)))))
(defun refuses (label area n scale &rest options)
  "Refused by the contract itself; any other condition fails the control."
  (multiple-value-bind (check root) (apply #'build area n (if (eq scale :default) 1 scale) options)
    (let ((c (handler-case (progn (check-call area check root scale) nil)
               (error (c) c))))
      (unless (and c (contract-refusal-p c))
        (error "Control failed: ~s observed ~s" label (and c (type-of c))))
      (push (list label (type-of c) (princ-to-string c)) *log*)
      (incf *pass*))))

(defun set-key (key value) (lambda (p) (setf (getf p key) value) p))
(defun drop-key (key) (lambda (p) (remf p key) p))

(defun area-controls (area)
  (let ((pauses (fourth (assoc area +areas+))))
    ;; Per-pause logical width at both scales: 1056 or 640 logical, times SCALE.
    (dolist (n pauses)
      (accepts (list area :v1 n) area n 1 :version 1)
      (dolist (s '(1 2)) (accepts (list area :v2 n s) area n s)))
    (let ((n (first (last pauses))) (lw (logical-width area (first (last pauses)))))
      (refuses (list area :v1-scale-2) area n 2 :version 1)
      (refuses (list area :v1-with-scale) area n 1 :version 1 :record (set-key :scale 1))
      (refuses (list area :v2-version-3) area n 1 :record (set-key :version 3))
      (dolist (s '(1 2))
        (let ((w (* s lw)) (h (* s 384)) (other (- 3 s)))
          (flet ((no (label &rest options) (apply #'refuses (list area label s) area n s options)))
            (no :record-scale :record (set-key :scale other))
            (no :native-scale :native (set-key :scale other))
            (no :native-physical-width :native (set-key :width (* 2 lw)))
            (no :other-width-rule :record (set-key :width (* s (if (= lw 640) 1056 640))))
            (no :decoded-length-field :record (set-key :decoded-length (- (* w h 4) 4)))
            (no :decoded-file-length :rgba-delta 4)
            (when (= s 2) (no :identify-logical :identify (format nil "~d 384" lw)))
            (no :identify-swapped :identify (format nil "~d ~d" h w))
            (no :identify-newline :identify (format nil "~d ~d~%" w h))
            (no :compared+1 :comparison (set-key :compared-pixels (1+ (* w h))))
            (no :compared-1 :comparison (set-key :compared-pixels (1- (* w h))))
            (no :pass-count-1 :comparison (set-key :different-pixels 1))
            (no :comparison-scale :comparison (set-key :scale other))
            (no :comparison-width :comparison (set-key :width (* other lw)))
            (no :last-difference :comparison (set-key :last-difference '(:x 0 :y 0)))
            (no :tolerance :comparison (set-key :tolerance 1))
            (no :missing-masked :comparison (drop-key :masked-pixels))
            (no :unknown-comparison-key :comparison (lambda (c) (append c (list :extra 1))))
            (no :duplicate-comparison-key :comparison (lambda (c) (list* :pass t c)))
            (no :missing-scale :record (drop-key :scale))
            (no :unknown-record-key :record (lambda (r) (append r (list :extra 1))))
            (no :duplicate-record-key :record (lambda (r) (list* :height h r))))))
      ;; A logical record substituted at scale 2 with self-consistent logical facts.
      (refuses (list area :logical-record-2) area n 2
               :record (lambda (r) (setf (getf r :width) lw (getf r :height) 384
                                         (getf r :decoded-length) (* lw 384 4))
                         r)
               :rgba-delta (- (* lw 384 4) (* 4 lw 384 4))
               :identify (format nil "~d 384" lw)
               :comparison (lambda (c) (setf (getf c :width) lw (getf c :height) 384
                                             (getf c :compared-pixels) (* lw 384))
                             c)))))

(defun screen-default-controls ()
  "Two-argument screen calls default to scale 1 and reach content validation."
  (accepts :screen-default-v2 :screen 1 :default)
  (accepts :screen-default-v1 :screen 1 :default :version 1)
  (refuses :screen-default-compared :screen 1 :default
           :comparison (set-key :compared-pixels (1+ (* 640 384))))
  (refuses :screen-default-v1-different :screen 1 :default :version 1
           :comparison (set-key :different-pixels 1))
  (multiple-value-bind (check root) (build :screen 1 2)
    (let ((c (handler-case (progn (check-call :screen check root :default) nil) (error (c) c))))
      (ok :screen-default-scale-2-record (and c (contract-refusal-p c))))))

(defun run-tests ()
  "Fixture roots are retained under *PARENT* for inspection."
  (setf *pass* 0 *log* nil
        *parent* (namestring (truename (concatenate 'string (sb-posix:mkdtemp "/tmp/vtphysical-XXXXXX") "/"))))
  (dolist (area '(:screen :double :charsets)) (area-controls area))
  (screen-default-controls)
  (format t "physical contract controls: ~d passed; roots retained in ~a~%" *pass* *parent*)
  (values *pass* (reverse *log*)))
