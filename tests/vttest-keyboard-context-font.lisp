;;;; Lazy one-shot font proof for keyboard case context: launch font transport,
;;;; RECORD-LOADED-FONT evidence, then the production decoder against the oracle.
;;;; Load as source after vttest-keyboard-driver.lisp and vttest-keyboard-launch.lisp.
(in-package #:vttest-keyboard-driver)
(require :sb-posix)

(unless (find-package '#:vttest-keyboard-font-evidence)
  (load (merge-pathnames "vttest-keyboard-font-evidence.lisp" *load-truename*)))
(unless (find-package '#:vttest-keyboard-decoder)
  (load (merge-pathnames "vttest-keyboard-decoder.lisp" *load-truename*)))

(define-condition font-proof-refusal (error)
  ((state :initarg :state :reader font-proof-state)
   (reason :initarg :reason :reader font-proof-reason)))

(defun font-refuse (state &rest reason)
  (error 'font-proof-refusal :state state :reason reason))

(defun font-title-p (title)
  "Launch title shape: Lispterm-vttest-<16 hex>-<pid>."
  (let* ((prefix "Lispterm-vttest-") (n (length prefix)))
    (and (stringp title) (> (length title) (+ n 17))
         (string= prefix title :end2 n)
         (every (lambda (c) (find c "0123456789abcdef")) (subseq title n (+ n 16)))
         (char= #\- (char title (+ n 16)))
         (every (lambda (c) (find c "0123456789")) (subseq title (+ n 17))))))

(defun check-font-directory (directory)
  "DIRECTORY itself, never a symlink target, is a directory owned by us with mode 0700."
  (let* ((stat (handler-case (sb-posix:lstat (string-right-trim "/" directory))
                 (sb-posix:syscall-error (e)
                   (font-refuse :fail :font-directory-unreadable (sb-posix:syscall-errno e)))))
         (mode (sb-posix:stat-mode stat)))
    (unless (= (logand mode sb-posix:s-ifmt) sb-posix:s-ifdir)
      (font-refuse :fail :font-directory-not-directory))
    (unless (= (sb-posix:stat-uid stat) (sb-posix:getuid))
      (font-refuse :fail :font-directory-owner (sb-posix:stat-uid stat)))
    (unless (= (logand mode #o7777) #o700)
      (font-refuse :fail :font-directory-mode (logand mode #o7777)))))

(defun load-proven-font (pinned)
  "Read PINNED once (regular, no symlink, exact pinned length), decode that snapshot
under the pin and return (values FONT ORACLE-OCTETS). PINNED is never read again."
  (unless (equal vttest-keyboard-decoder:*font-sha256*
                 vttest-keyboard-font-evidence::*pinned-sha256*)
    (font-refuse :fail :pin-constants-differ))
  (let* ((limit vttest-keyboard-font-evidence::*pinned-length*)
         (octets (handler-case (vttest-keyboard-font-evidence::read-regular pinned limit)
                   (error (e) (font-refuse :fail :pinned-font-unreadable (type-of e))))))
    (unless (eql (length octets) limit)
      (font-refuse :fail :pinned-length (length octets)))
    (multiple-value-bind (font oracle)
        (handler-case (vttest-keyboard-decoder:decode-pinned
                       octets vttest-keyboard-decoder:*font-sha256*)
          (vttest-keyboard-decoder:pin-refused () (font-refuse :fail :decoder-pin-refused)))
      (unless (and (equalp (lispterm.display::bitmap-font-bytes font) octets)
                   (equalp oracle octets))
        (font-refuse :fail :decoded-octets-differ))
      (values font oracle))))

(defun font-proof (transport root)
  "(values :PASS NIL ORACLE-OCTETS) or signal FONT-PROOF-REFUSAL."
  (unless (and (stringp root) (plusp (length root)))
    (font-refuse :unverified :no-run-root))
  (unless (char= #\/ (char root 0))
    (font-refuse :fail :run-root-not-absolute))
  (unless (char= #\/ (char root (1- (length root))))
    (font-refuse :fail :run-root-not-directory))
  (unless (and transport (listp transport)) (font-refuse :unverified :no-transport))
  (let ((directory (getf transport :directory))
        (pinned (getf transport :pinned-truename))
        (title (getf transport :expected-title)))
    (unless (equal directory (concatenate 'string root "font-evidence/"))
      (font-refuse :fail :font-directory-not-run-root))
    (check-font-directory directory)
    (unless title (font-refuse :unverified :no-expected-title))
    (unless (font-title-p title) (font-refuse :fail :expected-title-shape))
    (let ((canonical (ignore-errors
                      (namestring (truename (vttest-keyboard-decoder:font-path))))))
      (unless canonical (font-refuse :unverified :no-pinned-font))
      (unless (and (stringp pinned) (string= pinned canonical))
        (font-refuse :fail :pinned-truename-differs))
      (multiple-value-bind (verdict errors)
          (vttest-keyboard-font-evidence:verify-font-evidence directory title pinned)
        (cond ((equal errors '((:no-loaded-font-evidence)))
               (font-refuse :unverified :no-loaded-font-evidence))
              ((or errors (not (eq (getf verdict :claim) :loaded-octets-equal-pinned))
                   (not (equal (getf verdict :truename) pinned))
                   (not (equal (getf verdict :sha256) vttest-keyboard-decoder:*font-sha256*)))
               (font-refuse :fail :font-evidence (subseq errors 0 (min 4 (length errors))))))
        (multiple-value-bind (font bytes) (load-proven-font pinned)
          (unless (eql (length bytes) (getf verdict :length))
            (font-refuse :fail :oracle-length (length bytes)))
          (let* ((results (vttest-keyboard-decoder:compare-codes font bytes))
                 (bad (remove :match results :key #'cdr)))
            (unless (equal (mapcar #'car results) (loop for c from 32 to 126 collect c))
              (font-refuse :fail :code-sequence (length results)))
            (when bad (font-refuse :fail :glyph-mismatch (length bad)
                                   (subseq bad 0 (min 4 (length bad)))))
            (values :pass '() bytes)))))))

(defun make-font-proof (transport &optional root)
  "One closure per native run; nothing is checked here. The first call verifies and
caches; every call returns (values STATE REASONS ORACLE-OCTETS), octets only on :PASS."
  (let ((result nil))
    (lambda ()
      (unless result
        (setf result
              (handler-case (multiple-value-list (font-proof transport root))
                (font-proof-refusal (c)
                  (list (font-proof-state c) (list (font-proof-reason c)) nil))
                (error (c) (list :fail (list (list :font-proof-signal (type-of c))) nil)))))
      (destructuring-bind (state reasons octets) result
        (values state (copy-tree reasons)
                (and (eq state :pass) octets (copy-seq octets)))))))
