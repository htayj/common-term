;;;; Processed row-23 output proof for vttest 20251205 keyboard tests 5.4 and 5.5
;;;; (keyboard.c 1.48). Pure: one capture interval of the run's own output text.
;;;; Callers must already have established, through frame and DA validation:
;;;;  - OUTPUT is the complete accepted output transcript (CHECK-EVENTS passed);
;;;;  - the DA reply was ESC[?1;2c, so the VT100 tables of vttest-keyboard-cells hold;
;;;;  - REPLY-END is the output offset of the post-reply boundary;
;;;;  - each target is that capture's barrier :target-output, equal to its event :out.
;;;; Output text proves only what vttest wrote. It is not native, pixel, font or
;;;; timing proof, and nothing here supplies provenance.
(load (merge-pathnames "vttest-keyboard-cells.lisp" *load-truename*))

(defpackage #:vttest-keyboard-row-output
  (:use #:cl)
  (:local-nicknames (#:c #:vttest-keyboard-cells))
  (:export #:row-output-reasons #:case-row-output-errors))
(in-package #:vttest-keyboard-row-output)

(defun csi (body) (format nil "~c[~a" (code-char 27) body))

(defun row-erase ()
  "vt_move(23,1) then vt_el(0), as both reviewed fixtures write it."
  (concatenate 'string (csi "23;1H") (csi "0K")))

(defun key-redraw (label phase k)
  "Bytes after the erase for the K-th pressed key of PHASE: reverse echo, reverse
( NAME key), then the plain label at its keyboard position, as REPLAY draws them."
  (destructuring-bind (keys other pressed &rest more) (c::case-of label)
    (declare (ignore other more))
    (destructuring-bind (r col text name) (nth (nth k pressed) keys)
      (concatenate 'string (csi "23;1H") (csi "7m") (c:echo (c:key-bytes label phase k)) (csi "m")
                   (csi "7m") (format nil " (~a key) " name) (csi "m")
                   (csi (format nil "~d;~dH" (+ 1 (* 2 r)) (+ 1 col))) text))))

(defun tab-redraw ()
  (concatenate 'string (csi "23;1H") (csi "7m") (c:echo (string #\Tab)) (csi "m")))

(defparameter +hold+ "Push <RETURN>")

(defun erases (output start end)
  "Offsets of every row-23 erase wholly inside [START, END)."
  (let ((e (row-erase)))
    (loop for i = (search e output :start2 start :end2 end)
            then (search e output :start2 (1+ i) :end2 end)
          while i collect i)))

(defun body (i) (+ i (length (row-erase))))

(defun body-p (output i text end)
  "TEXT follows the erase at I and ends inside the interval."
  (let* ((b (body i)) (e (+ b (length text))))
    (and (<= e end) (string= text output :start2 b :end2 e))))

(defun clean-tail-p (output from end)
  "No further erase or reverse text after the proved redraw."
  (notany (lambda (s) (search s output :start2 from :end2 end)) (list (csi "0K") (csi "7m"))))

(defun key-reasons (output start end redraw after-tab)
  "The final erase carries REDRAW. AFTER-TAB: the first key of a later phase, whose
interval also holds exactly the previous phase's TAB erase and echo."
  (let* ((at (erases output start end)) (final (car (last at))))
    (cond ((null at) '(:no-row-erase))
          ((not (body-p output final redraw end))
           (let ((avail (- end (body final))))
             (list (cond ((body-p output final (tab-redraw) end) :tab-residue)
                         ((and (< avail (length redraw))
                               (string= output redraw :start1 (body final) :end1 end :end2 avail))
                          :cut-before-redraw)
                         (t :wrong-redraw)))))
          ((not (clean-tail-p output (+ (body final) (length redraw)) end)) '(:later-row-output))
          ((/= (length at) (if after-tab 2 1)) '(:row-erase-count))
          ((and after-tab (not (body-p output (first at) (tab-redraw) end))) '(:no-tab-echo)))))

(defun pause-reasons (output start end)
  "The final TAB erase and echo ' <9> ', then the hold erase and Push <RETURN>."
  (let ((at (erases output start end)))
    (cond ((/= (length at) 2) '(:row-erase-count))
          ((not (body-p output (first at) (tab-redraw) end)) '(:no-tab-echo))
          ((not (body-p output (second at) +hold+ end)) '(:no-hold-prompt))
          ((not (clean-tail-p output (+ (body (second at)) (length +hold+)) end))
           '(:later-row-output)))))

(defun interval-p (output start end)
  (and (stringp output) (typep start '(integer 0)) (typep end '(integer 0))
       (< start end) (<= end (length output))))

(defun row-output-reasons (label id output start end)
  "NIL when OUTPUT[START, END) holds the reviewed row-23 erase and redraw for capture
ID of LABEL, otherwise a list of reason keywords. START is the previous capture's
:target-output, or REPLY-END for the first capture; END is this capture's."
  (let* ((ids (ignore-errors (c:capture-ids label)))
         (i (position id ids :test #'equal)))
    (cond ((not (interval-p output start end)) '(:malformed-interval))
          ((null i) '(:unknown-capture))
          ((equal id "pause01") (pause-reasons output start end))
          (t (multiple-value-bind (phase k) (floor i (length (third (c::case-of label))))
               (key-reasons output start end (key-redraw label phase k)
                            (and (zerop k) (plusp phase))))))))

(defun case-row-output-errors (label output reply-end targets)
  "Every (ID . REASONS) failure of LABEL. TARGETS lists each capture's :target-output
in CAPTURE-IDS order; the first interval starts at REPLY-END, each later one at the
previous target."
  (let ((ids (ignore-errors (c:capture-ids label))))
    (cond ((null ids) '(:unknown-case))
          ((not (eql (ignore-errors (list-length targets)) (length ids))) '(:target-count))
          (t (let ((start reply-end))
               (loop for id in ids for end in targets
                     for reasons = (row-output-reasons label id output start end)
                     do (setf start end)
                     when reasons collect (cons id reasons)))))))
