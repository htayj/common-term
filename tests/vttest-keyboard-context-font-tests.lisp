;;;; Source controls for vttest-keyboard-context-font.lisp: the lazy MAKE-FONT-PROOF
;;;; closure and LOAD-PROVEN-FONT. Every reader, verifier and decoder is a counted stub or
;;;; a tripwire (vttest-keyboard-context-font-fixtures.lisp), so these prove control flow
;;;; only; the real pinned decode, wrong pin and flipped byte belong to the decoder RUN.
;;;; Load after vttest-keyboard-driver-tests.lisp and vttest-keyboard-context-font.lisp.
;;;; Entry point: (CONTEXT-FONT-CONTROLS).
(in-package :vttest-keyboard-driver-tests)
(load (merge-pathnames "vttest-keyboard-context-font-fixtures.lisp" *load-truename*))

(defun expect-proof (tag transport root state reasons &optional overrides)
  "First call of a fresh closure gives STATE and REASONS, no octets, without any I/O
beyond OVERRIDES."
  (in-font-world overrides
    (destructuring-bind (s r o) (proof-values transport root)
      (check (and (eq s state) (equal r reasons) (null o)) (list :font-proof tag s r))
      (font-io-calls))))

(defun closure-controls ()
  (let ((snapshot (font-snapshot)) (transport (font-transport)))
    (in-font-world '()
      (k::make-font-proof transport *font-root*)
      (k::make-font-proof nil)
      (check (zerop (font-io-calls)) :font-closure-creation-reads-nothing))
    (let ((proof (in-font-world '() (k::make-font-proof transport *font-root*))))
      (in-font-world (pass-world snapshot)
        (let ((calls (loop repeat 3 collect (multiple-value-list (funcall proof)))))
          (check (and (every (lambda (v) (and (eq :pass (first v)) (null (second v))
                                              (equalp snapshot (third v))))
                             calls)
                      (not (eq (third (first calls)) (third (second calls))))
                      (not (eq snapshot (third (first calls)))))
                 :font-closure-pass-fresh-octet-copies)
          (check (equal '(1 1 1 1 1 1)
                        (mapcar (lambda (e) (font-calls (car e))) *font-io*))
                 :font-closure-evaluates-once)))
      (in-font-world '()
        (check (and (eq :pass (funcall proof)) (zerop (font-io-calls)))
               :font-closure-retains-pass-without-io))
      (let ((other (in-font-world '() (k::make-font-proof transport *font-root*))))
        (in-font-world '()
          (destructuring-bind (s r o) (multiple-value-list (funcall other))
            (check (and (eq s :fail) (equal r '((:font-proof-signal font-tripwire))) (null o)
                        (eq :pass (funcall proof)))
                   :font-closures-independent)))
        (in-font-world (pass-world snapshot)
          (check (and (eq :fail (funcall other)) (zerop (font-io-calls)))
                 :font-closure-retains-refusal))))))

(defun root-and-title-controls ()
  (let ((none '()) (dir (list :directory (font-returning nil))))
    (check (zerop (expect-proof :no-root (font-transport) nil :unverified '((:no-run-root))))
           :font-no-root-no-io)
    (check (zerop (expect-proof :relative (font-transport :root "run/x/") "run/x/"
                                :fail '((:run-root-not-absolute)) none))
           :font-relative-root-no-io)
    (check (zerop (expect-proof :no-slash (font-transport) "/run/vttest-font-control"
                                :fail '((:run-root-not-directory)) none))
           :font-root-not-directory-no-io)
    (dolist (pid (list (string (code-char #x0664)) "+42" "4 2" ""))
      (check (= 1 (expect-proof (list :pid pid)
                                (font-transport :title (concatenate 'string
                                                                    "Lispterm-vttest-0123456789abcdef-"
                                                                    pid))
                                *font-root* :fail '((:expected-title-shape)) dir))
             (list :font-pid-refused-before-verifier pid)))))

(defun verifier-and-code-controls ()
  (let* ((snapshot (font-snapshot)) (good (pass-world snapshot)) (n (length snapshot))
         (codes (ascii-codes)))
    (flet ((with-verify (verdict errors)
             (append good (list :verify (font-returning verdict errors) :read nil))))
      (expect-proof :verifier-sha (font-transport) *font-root* :fail '((:font-evidence nil))
                    (with-verify (font-verdict n :sha (wrong-sha)) nil))
      (expect-proof :no-evidence (font-transport) *font-root*
                    :unverified '((:no-loaded-font-evidence))
                    (with-verify nil '((:no-loaded-font-evidence))))
      (expect-proof :malformed-evidence (font-transport) *font-root*
                    :fail '((:font-evidence ((:records-differ 2))))
                    (with-verify nil '((:records-differ 2))))
      (expect-proof :oracle-length (font-transport) *font-root* :fail `((:oracle-length ,n))
                    (pass-world snapshot :verdict (font-verdict (1- n)))))
    (loop for (tag bad) in `((:reversed ,(reverse codes))
                             (:duplicate ,(cons 32 (butlast codes)))
                             (:short ,(rest codes))
                             (:shifted ,(mapcar #'1+ codes)))
          do (expect-proof tag (font-transport) *font-root*
                           :fail `((:code-sequence ,(length bad)))
                           (pass-world snapshot :codes bad)))))

(defun load-proven-font-controls ()
  (let* ((snapshot (font-snapshot)) (good (pass-world snapshot))
         (limit vttest-keyboard-font-evidence::*pinned-length*))
    (flet ((expect (tag reason overrides reads decodes)
             (multiple-value-bind (result read-args r d) (load-font overrides)
               (check (and (equal result (cons :fail reason)) (= r reads) (= d decodes)
                           (or (zerop r) (equal read-args (list *fixture-truename* limit))))
                      (list :load-proven-font tag result r d)))))
      (let ((vttest-keyboard-decoder:*font-sha256* (wrong-sha)))
        (expect :pin-constants '(:pin-constants-differ) '() 0 0))
      (expect :read-refused '(:pinned-font-unreadable vttest-keyboard-font-evidence::refusal)
              (list :read (font-signalling 'vttest-keyboard-font-evidence::refusal
                                           :reason '(:not-regular-file "x")))
              1 0)
      (expect :short `(:pinned-length ,(1- limit))
              (list :read (font-returning (subseq snapshot 1))) 1 0)
      (expect :long `(:pinned-length ,(1+ limit))
              (list :read (font-returning (concatenate '(vector (unsigned-byte 8)) snapshot #(0))))
              1 0)
      (expect :decoder-pin '(:decoder-pin-refused)
              (append good (list :decode (font-signalling 'vttest-keyboard-decoder:pin-refused
                                                          :details '(:stub t))))
              1 1)
      (expect :font-bytes '(:decoded-octets-differ)
              (pass-world snapshot :font-bytes (flipped snapshot)) 1 1)
      (expect :oracle '(:decoded-octets-differ)
              (pass-world snapshot :oracle (flipped snapshot)) 1 1)
      (multiple-value-bind (result read-args r d io decode-args) (load-font good)
        (destructuring-bind (&optional font oracle) result
          (check (and (typep font 'lispterm.display::bitmap-font) (equalp oracle snapshot)
                      (equalp (lispterm.display::bitmap-font-bytes font) snapshot)
                      (equal read-args (list *fixture-truename* limit))
                      (eq (first decode-args) snapshot)
                      (equal (second decode-args) vttest-keyboard-decoder:*font-sha256*)
                      (= 1 r d) (= 2 io))
                 :load-proven-font-one-snapshot-no-rereads))))))

(defun context-font-controls ()
  "Stubbed source controls only; never a real font or native proof."
  (let ((originals (font-functions)))
    (closure-controls)
    (root-and-title-controls)
    (verifier-and-code-controls)
    (load-proven-font-controls)
    (check (every #'eq originals (font-functions)) :font-stubs-restored)))
