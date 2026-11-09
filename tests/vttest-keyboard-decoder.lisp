;;;; Keyboard pixel plan slice C: production read-otb/otb-glyph versus the cursor
;;;; oracle's glyph-rows for codes 32..126. Load as source; RUN returns T on success.
(require :asdf)

(let ((root (uiop:pathname-parent-directory-pathname
             (uiop:pathname-directory-pathname *load-truename*))))
  (dolist (file '("src/display/package.lisp" "src/display/font-otb.lisp"
                  "tests/vttest-cursor-oracle.lisp"))
    (load (merge-pathnames file root)))
  (unless (find-package '#:vttest-area)
    (load (merge-pathnames "tests/vttest-area.lisp" root))))

(defpackage #:vttest-keyboard-decoder
  (:use #:cl)
  (:local-nicknames (#:o #:vttest-cursor-oracle) (#:d #:lispterm.display))
  (:export #:*font-sha256* #:font-path #:pin-refused #:pinned-sha256 #:decode-pinned
           #:load-pinned #:production-rows #:oracle-rows #:code-status #:compare-codes #:run))
(in-package #:vttest-keyboard-decoder)

(defparameter *root*
  (uiop:pathname-parent-directory-pathname (uiop:pathname-directory-pathname *load-truename*)))
(defparameter *font-sha256* "0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca")
(defparameter *codes* (loop for code from 32 to 126 collect code))
(defvar *glyph-comparisons* 0 "Count of CODE-STATUS calls, used by the pin-order control.")

(defun font-path () (merge-pathnames ".build/deps/share/fonts/terminus/ter-u16n.otb" *root*))

(load (merge-pathnames "vttest-keyboard-decoder-pin.lisp" *load-truename*))

(defun production-rows (font code)
  "Pack production otb-glyph's 128 row-major bits into 16 MSB-first row octets."
  (let ((bits (d::otb-glyph font (code-char code)))
        (rows (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (assert (and (= 8 (d:font-width font)) (= 16 (d:font-height font)) (= 128 (length bits))))
    (dotimes (y 16 rows)
      (dotimes (x 8)
        (setf (ldb (byte 1 (- 7 x)) (aref rows y)) (aref bits (+ x (* 8 y))))))))

(defun oracle-rows (bytes code)
  (coerce (o:glyph-rows bytes (code-char code)) '(vector (unsigned-byte 8))))

(defun code-status (font bytes code &key (oracle #'oracle-rows))
  "Classify CODE as :MATCH, :MISMATCH, :MISSING or :ORACLE-ERROR. :MISSING means the
production glyph id is 0, outside the oracle strike, or differs from the oracle cmap.
Only :MATCH is success, so blank output from a missing or malformed glyph never passes."
  (incf *glyph-comparisons*)
  (let ((gid (d::font-glyph-index font code))
        (info (o:font-info bytes)))
    (cond ((or (zerop gid) (not (<= (getf info :first) gid (getf info :last)))
               (not (eql gid (ignore-errors (o::cmap-glyph bytes code)))))
           :missing)
          (t (let ((expected (handler-case (funcall oracle bytes code) (error () nil))))
               (cond ((null expected) :oracle-error)
                     ((equalp expected (production-rows font code)) :match)
                     (t :mismatch)))))))

(defun compare-codes (font bytes &key (oracle #'oracle-rows) (codes *codes*))
  "Alist of (CODE . STATUS) for CODES."
  (loop for code in codes collect (cons code (code-status font bytes code :oracle oracle))))

(defun run (&key (path (font-path)) (stream *standard-output*))
  (let ((pass 0) (fail 0)
        (zero (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (flet ((check (name ok &optional detail)
             (if ok (incf pass) (incf fail))
             (format stream "~:[FAIL~;PASS~] ~a~@[ ~s~]~%" ok name (unless ok detail))))
      (let* ((*glyph-comparisons* 0)
             (wrong (concatenate 'string "1" (subseq *font-sha256* 1)))
             (refused (handler-case
                          (multiple-value-bind (font bytes) (load-pinned path wrong)
                            (compare-codes font bytes) nil)
                        (pin-refused () t))))
        (check "wrong pin refused" refused)
        (check "wrong pin made no glyph comparison" (zerop *glyph-comparisons*)
               *glyph-comparisons*)
        (let ((flipped (o:read-bytes path)))
          (setf (aref flipped 100) (logxor 1 (aref flipped 100)))
          (check "flipped byte refused before decoding"
                 (handler-case (progn (decode-pinned flipped *font-sha256*) nil)
                   (pin-refused () t)))
          (check "flipped byte made no glyph comparison" (zerop *glyph-comparisons*)
                 *glyph-comparisons*)))
      (multiple-value-bind (font bytes) (load-pinned path *font-sha256*)
        (check "font pin before comparison" t)
        (check "font octets equal oracle octets" (equalp (d::bitmap-font-bytes font) bytes))
        (check "font octets not aliased to oracle" (not (eq (d::bitmap-font-bytes font) bytes)))
        (check "memory SHA-256 equals external pinned SHA-256"
               (equal (vttest-area::sha256 bytes)
                      (handler-case (pinned-sha256 path *font-sha256*) (pin-refused () nil))))
        (let ((oracle-bytes (copy-seq bytes))
              (font-bytes (copy-seq (d::bitmap-font-bytes font)))
              (results (compare-codes font bytes)))
          (check "95 codes compared" (= 95 (length results)) (length results))
          (dolist (r results)
            (check (format nil "code ~d" (car r)) (eq :match (cdr r)) (cdr r)))
          (check "space status match" (eq :match (code-status font bytes 32)))
          (check "space production gid nonzero" (plusp (d::font-glyph-index font 32)))
          (check "space oracle all-zero" (equalp zero (oracle-rows bytes 32)))
          (check "space production all-zero" (equalp zero (production-rows font 32)))
          (let ((original (make-hash-table)) (copy (make-hash-table)))
            (dolist (code *codes*)
              (setf (gethash code original) (oracle-rows bytes code)
                    (gethash code copy) (copy-seq (gethash code original))))
            (let ((a (copy-seq (gethash 65 original))))
              (setf (aref (gethash 65 copy) 7) (logxor #x10 (aref (gethash 65 copy) 7)))
              (let ((bad (remove :match (compare-codes font bytes :oracle
                                                       (lambda (b code) (declare (ignore b))
                                                         (gethash code copy)))
                                 :key #'cdr)))
                (check "one-bit A mutation mismatches exactly code 65"
                       (equal bad '((65 . :mismatch))) bad))
              (check "original oracle A untouched"
                     (and (equalp a (gethash 65 original)) (equalp a (oracle-rows bytes 65))))))
          (check "oracle error is not blank success"
                 (eq :oracle-error (code-status font bytes 32 :oracle
                                                (lambda (b c) (declare (ignore b c))
                                                  (error "malformed")))))
          (let* ((broken (copy-seq bytes)) (info (o:font-info broken))
                 (p (+ (getf info :data)
                       (* 21 (- (o::cmap-glyph broken 32) (getf info :first))))))
            (setf (aref broken p) 0)
            (check "malformed space header is not blank success"
                   (eq :oracle-error (code-status font broken 32))))
          (check "unmapped U+FFFF is missing" (eq :missing (code-status font bytes #xFFFF)))
          (check "oracle octets untouched" (equalp oracle-bytes bytes))
          (check "production octets untouched" (equalp font-bytes (d::bitmap-font-bytes font)))
          (check "font pin after comparison"
                 (handler-case (pinned-sha256 path *font-sha256*) (pin-refused () nil))))))
    (format stream "~d passed, ~d failed~%" pass fail)
    (values (zerop fail) pass fail)))
