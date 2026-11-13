(in-package #:lispterm.gpu)

(defun open-context (&key software device)
  "Create an owner-thread ES3 context. SOFTWARE must explicitly permit software GL.
DEVICE is NIL (discover) or an absolute DRM render-node pathname. No desktop handles."
  (declare (ignore software device))
  (unavailable 'open-context))

(defgeneric assert-context-owner (context)
  (:documentation "Reject use after close, from another thread, or after fork.")
  (:method (context) (declare (ignore context)) (unavailable 'assert-context-owner)))
(defgeneric close-context (context)
  (:documentation "Destroy on its owner thread; idempotent after a successful close.")
  (:method (context) (declare (ignore context)) (unavailable 'close-context)))
(defgeneric context-info (context)
  (:documentation "Return a plist of vendor, renderer, version, driver, device and software-p.")
  (:method (context) (declare (ignore context)) (unavailable 'context-info)))
