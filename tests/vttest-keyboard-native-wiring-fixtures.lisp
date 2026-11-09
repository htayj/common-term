;;;; Native wiring fixtures: one stubbed world for NATIVE-MAIN and NATIVE-EVIDENCE.
;;;; Launch, request, manifest, cleanup, receipt, byte, identity, proof and pixel
;;;; consumers are logged stubs restored by CALL-WITH-STUBS; the only file effect is
;;;; NATIVE-MAIN's pixel-scratch directory under a fresh TEMP-ROOT. No process,
;;;; display, PNG, font or decoder work. Load after vttest-keyboard-native-main.lisp
;;;; and vttest-keyboard-context-tests.lisp (CALL-WITH-STUBS).
(in-package :vttest-keyboard-driver-tests)

(dolist (f '(k::native-main k::guarded-native-evidence k::native-evidence k::proof-of
             k::pixels-of k::font-result k::make-case-context k::pixel-status
             call-with-stubs temp-root))
  (unless (fboundp f) (error "Native wiring fixtures need ~s loaded first" f)))

(defparameter *wiring-stubbed*
  '(k::native-arguments vttest-area:read-data k::validate k::native-put
    vttest-cursor-driver::bytes k::digest k::launch-keyboard-gate k::native-linkage
    k::make-font-proof k::consume-gate-cleanup k::validate-receipt k::case-bytes
    k::case-identity k::case-proof k::case-pixels k::native-evidence)
  "Every function the wiring controls rebind or unbind.")
(defparameter *wiring-transport* (list :transport :stub) "The launch's fourth value.")
(defparameter *wiring-octets* (coerce '(0 24 36 66) '(vector (unsigned-byte 8))))
(defvar *wiring-log* nil "(KEY . ARGS) per stub call, newest first.")
(defvar *wiring-contexts* nil "(LABEL . CTX) per stub CASE-PROOF, newest first.")
(defvar *wiring-fail* nil "Capture name whose stub pixel entry fails.")

(defun wiring-returning (&rest values)
  (lambda (&rest args) (declare (ignore args)) (values-list values)))
(defun wiring-calls (key log)
  "Argument lists of KEY's calls in LOG, oldest first."
  (reverse (mapcar #'cdr (remove key log :key #'car :test-not #'eq))))
(defun wiring-name (label id)
  (format nil "~2,'0d-~a-~a" (1+ (position id (k::capture-ids label) :test #'equal)) label id))
(defun wiring-originals (label)
  (mapcar (lambda (id) (cons id (wiring-name label id))) (k::capture-ids label)))

(defun wiring-proof (label dir bytes font-proof)
  "CASE-PROOF stand-in: timing passes, prerequisites carry the font closure's state."
  (declare (ignore dir bytes))
  (let ((ctx (k::make-case-context :label label)))
    (push (cons label ctx) *wiring-contexts*)
    (values :pass nil (k::font-result font-proof) nil ctx)))

(defun wiring-pixels (label ctx prereq timing &key scratch)
  "CASE-PIXELS stand-in: entries only for PREREQ and TIMING :PASS; *WIRING-FAIL* fails."
  (declare (ignore ctx scratch))
  (if (and (eq prereq :pass) (eq timing :pass))
      (let ((entries (loop for (id . name) in (wiring-originals label)
                           collect (list :name name :label label :id id
                                         :pixels (if (equal name *wiring-fail*) :fail :pass)))))
        (values (if (find :fail entries :key (lambda (e) (getf e :pixels))) :fail :pass)
                entries nil))
      (values :unverified nil '(:proof-not-passed))))

(defun wiring-stubs (root &key (launch :four) (cleanup :pass)
                            (bytes (wiring-returning :pass nil))
                            (proof #'wiring-proof) (pixels #'wiring-pixels)
                            (font (lambda (transport root) (declare (ignore transport root))
                                    (lambda () (push (list :font-call) *wiring-log*)
                                      (values :pass nil *wiring-octets*)))))
  "Logged bindings for one NATIVE-MAIN run in ROOT. LAUNCH :THREE is the old
three-value launch. PROOF, PIXELS or FONT :MISSING omits that binding; the caller
unbinds it with WIRING-WITHOUT."
  (flet ((logged (key fn)
           (unless (eq fn :missing)
             (cons key (lambda (&rest args)
                         (push (cons key args) *wiring-log*) (apply fn args))))))
    (remove nil
            (list (logged 'k::native-arguments (wiring-returning root))
                  (logged 'vttest-area:read-data (wiring-returning '(:backend "x11") "manifest-hash"))
                  (logged 'k::validate (wiring-returning t))
                  (logged 'k::native-put (wiring-returning t))
                  (logged 'vttest-cursor-driver::bytes (wiring-returning *wiring-octets*))
                  (logged 'k::digest (wiring-returning "request-hash"))
                  (logged 'k::launch-keyboard-gate
                          (if (eq launch :four)
                              (wiring-returning '(:expected) '(:exit-code 0) nil *wiring-transport*)
                              (wiring-returning '(:expected) '(:exit-code 0) nil)))
                  (logged 'k::native-linkage (wiring-returning nil))
                  (logged 'k::make-font-proof font)
                  (logged 'k::consume-gate-cleanup (wiring-returning cleanup nil '(:projection)))
                  (logged 'k::validate-receipt (wiring-returning t))
                  (logged 'k::case-bytes bytes)
                  (logged 'k::case-identity
                          (lambda (label dir) (declare (ignore dir))
                            (values :pass (wiring-originals label))))
                  (logged 'k::case-proof proof)
                  (logged 'k::case-pixels pixels)))))

(defun wiring-put (name log)
  "Value NATIVE-MAIN last wrote to a path ending in NAME."
  (second (find-if (lambda (args)
                     (let ((p (first args)))
                       (and (>= (length p) (length name))
                            (string= name p :start2 (- (length p) (length name))))))
                   (reverse (wiring-calls 'k::native-put log)))))

(defun wiring-run (&rest keys)
  "NATIVE-MAIN under KEYS' stubs in a fresh root. Values: exit, receipt,
observations, log, contexts, root."
  (let* ((root (temp-root)) (*wiring-log* nil) (*wiring-contexts* nil)
         (exit (call-with-stubs (apply #'wiring-stubs root keys)
                                (lambda ()
                                  (k::native-main "x11" (string-right-trim "/" root)
                                                  (concatenate 'string root "MANIFEST.sexp"))))))
    (values exit (wiring-put k::+receipt+ *wiring-log*)
            (wiring-put "NATIVE-OBSERVATIONS.sexp" *wiring-log*)
            *wiring-log* *wiring-contexts* root)))

(defun wiring-without (symbol thunk)
  "THUNK with SYMBOL's function unbound; always restored."
  (let ((old (fdefinition symbol)))
    (unwind-protect (progn (fmakunbound symbol) (funcall thunk))
      (setf (fdefinition symbol) old))))

(defparameter *wiring-sources*
  (mapcar (lambda (name) (merge-pathnames name *load-truename*))
          '("vttest-keyboard-native-main.lisp" "vttest-keyboard-native-evidence.lisp"
            "vttest-keyboard-native-summary.lisp")))

(defun wiring-text (path)
  (with-open-file (s path :external-format :utf-8)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun wiring-signal (&rest args) (declare (ignore args)) (error "stub evidence"))
