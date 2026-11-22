;;;; Native X11 admission: independent map interceptor, XRes client and PRIMARY owner.
(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(require :sb-posix)
(require :sb-bsd-sockets)
(asdf:load-system "xclhb")
(asdf:load-system "babel")

(defpackage #:common-term.x11-admission (:use #:cl))
(in-package #:common-term.x11-admission)

(defparameter *artifacts* (pathname (first (uiop:command-line-arguments))))
(defvar *display*)
(defvar *server*)
(defvar *terminal*)

(defun artifact (name)
  (merge-pathnames name *artifacts*))

(defun check (condition message &rest args)
  (unless condition (error "~?" message args)))

(defun passed (message &rest args)
  (format t "PASS: ~?~%" message args)
  (finish-output))

(defun bounded (seconds label function)
  (handler-case (sb-ext:with-timeout seconds (funcall function))
    (sb-ext:timeout () (error "Timed out: ~A" label))))

(defun wait-for (label function &optional (seconds 10))
  (bounded seconds label
           (lambda ()
             (loop for value = (funcall function) when value return value
                   do (sleep 0.01)))))

(defun stop-process (process)
  (when process
    (when (uiop:process-alive-p process)
      (uiop:terminate-process process)
      (handler-case (sb-ext:with-timeout 2 (uiop:wait-process process))
        (sb-ext:timeout () (uiop:terminate-process process :urgent t))))
    (uiop:wait-process process)))

(defun connect-display ()
  (let ((client (xclhb:x-connect nil *display*)))
    (check client "Cannot connect to private display ~A" *display*)
    (xclhb:set-default-error-handler
     client (lambda (event) (error "Unexpected X error: ~S" event)))
    client))

(defmacro with-connection ((client) &body body)
  `(let ((,client (connect-display)))
     (unwind-protect (progn ,@body) (xclhb:x-close ,client))))

(defun root-window (client)
  (xclhb:screen-root (aref (xclhb:setup-roots (xclhb:client-server-information client)) 0)))

(defun intern-x-atom (client name)
  (xclhb:intern-atom-reply-atom (xclhb:intern-atom-sync client 0 (length name) (octets name))))

(defun get-card32 (bytes offset)
  (loop for i below 4 sum (ash (aref bytes (+ offset i)) (* 8 i))))

(defun set-card32 (bytes offset word)
  (dotimes (i 4) (setf (aref bytes (+ offset i)) (ldb (byte 8 (* 8 i)) word))))

(defun octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun get-property (client window property type format count)
  (let ((reply (xclhb:get-property-sync client 0 window (intern-x-atom client property) 0 0 128)))
    (check (and (= (xclhb:get-property-reply-type reply) (intern-x-atom client type))
                (= (xclhb:get-property-reply-format reply) format)
                (= (xclhb:get-property-reply-value-len reply) count)
                (zerop (xclhb:get-property-reply-bytes-after reply)))
           "Pre-map ~A: expected ~A/~D x ~D, got ~S" property type format count reply)
    (xclhb:get-property-reply-value reply)))

(defun extension-opcode (client name)
  (let ((reply (xclhb:query-extension-sync client (length name) (octets name))))
    (check (plusp (xclhb:query-extension-reply-present reply))
           "x11-admission prerequisite: Xvfb must provide ~A" name)
    (xclhb:query-extension-reply-major-opcode reply)))

;;; xclhb implements core X11, not XRes. Use these two XRes 1.2 requests
;;; on a dedicated little-endian connection with no outstanding core requests.
(defun xres-reply (stream opcode minor request)
  (setf (aref request 0) opcode (aref request 1) minor
        (aref request 2) (/ (length request) 4))
  (write-sequence request stream)
  (finish-output stream)
  (let ((header (make-array 32 :element-type '(unsigned-byte 8))))
    (check (and (= (read-sequence header stream) 32) (= (aref header 0) 1))
           "XRes request ~D did not return a reply" minor)
    (let ((length (* 4 (get-card32 header 4))))
      (check (<= length 4096) "Unexpected XRes reply length ~D" length)
      (let ((data (make-array length :element-type '(unsigned-byte 8))))
        (check (= (read-sequence data stream) length) "Short XRes reply")
        (values header data)))))

(defun xres-pid (window)
  (with-connection (client)
    (let* ((opcode (extension-opcode client "X-Resource"))
           (stream (xclhb:client-stream client))
           (version (make-array 8 :element-type '(unsigned-byte 8) :initial-contents '(0 0 0 0 1 2 0 0)))
           (reply (xres-reply stream opcode 0 version))
           (major (xclhb::read-card16 reply 8)) (minor (xclhb::read-card16 reply 10))
           (request (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
      (check (and (= major 1) (>= minor 2))
             "x11-admission prerequisite: XRes >= 1.2 required, got ~D.~D" major minor)
      (set-card32 request 4 1) ; one client spec
      (set-card32 request 8 window)
      (set-card32 request 12 2) ; LOCAL_CLIENT_PID
      (multiple-value-bind (header data) (xres-reply stream opcode 4 request)
        (check (and (= (get-card32 header 8) 1) (= (length data) 16)
                    (= (get-card32 data 4) 2) (= (get-card32 data 8) 4)
                    (plusp (get-card32 data 12)))
               "x11-admission prerequisite: XRes ~D.~D did not return one LOCAL_CLIENT_PID for #x~X: ~S"
               major minor window data)
        (values (get-card32 data 12) major minor)))))

(defun verify-identity (client window name)
  (check (zerop (xclhb:get-window-attributes-reply-map-state
                 (xclhb:get-window-attributes-sync client window)))
         "Expected window #x~X to be unmapped at MapRequest" window)
  (let* ((expected-class (octets (format nil "~A~C~A~C" name #\Nul name #\Nul)))
         (hostname (octets (string-right-trim (list #\Newline) (uiop:run-program (list "uname" "-n") :output :string))))
         (class (get-property client window "WM_CLASS" "STRING" 8 (length expected-class)))
         (pid (get-card32 (get-property client window "_NET_WM_PID" "CARDINAL" 32 1) 0))
         (machine (get-property client window "WM_CLIENT_MACHINE" "STRING" 8 (length hostname)))
         (exec-pid (uiop:process-info-pid *terminal*)))
    (check (equalp class expected-class) "Pre-map WM_CLASS: expected ~S, got ~S" expected-class class)
    (check (equalp machine hostname) "Pre-map WM_CLIENT_MACHINE: expected ~S, got ~S" hostname machine)
    (check (= pid exec-pid) "Pre-map _NET_WM_PID ~D differs from exec PID ~D" pid exec-pid)
    (multiple-value-bind (local-pid major minor) (xres-pid window)
      (check (= pid local-pid) "Pre-map _NET_WM_PID ~D differs from XRes owner PID ~D" pid local-pid)
      (passed "pre-map ~A WM_CLASS, hostname and exec/XRes ~D.~D PID ~D"
              name major minor pid))))

(defun selection-owner (client primary)
  (xclhb:get-selection-owner-reply-owner (xclhb:get-selection-owner-sync client primary)))

(defun with-primary (window text fallback function)
  ;; Different X connection than the map interceptor or terminal. It owns an
  ;; unmapped window and explicitly serves just the expected target sequence.
  (with-connection (owner)
    (let* ((id (xclhb:allocate-resource-id owner))
           (primary (intern-x-atom owner "PRIMARY"))
           (utf8 (intern-x-atom owner "UTF8_STRING"))
           (string (intern-x-atom owner "STRING"))
           (targets (cond (fallback (list utf8 string)) (text (list utf8))))
           (original-time nil))
      (apply #'xclhb:create-window owner 0 id (root-window owner) 0 0 1 1 0 1 0 0
             (make-list 15 :initial-element 0))
      (xclhb:set-selection-owner owner id primary 0)
      (check (= (selection-owner owner primary) id) "Failed to own PRIMARY")
      (xclhb:set-event-handler
       owner xclhb:+selection-request-event+
       (lambda (event)
         (let ((target (xclhb:selection-request-event-target event))
               (time (xclhb:selection-request-event-time event))
               (property (xclhb:selection-request-event-property event)))
           (check (and (eql target (pop targets))
                       (= (xclhb:selection-request-event-owner event) id)
                       (= (xclhb:selection-request-event-selection event) primary)
                       (= (xclhb:selection-request-event-requestor event) window)
                       (plusp property) (plusp time)
                       (or (not original-time) (= original-time time)))
                  "Unexpected PRIMARY conversion request ~S" event)
           (setf original-time time)
           (cond ((and fallback (= target utf8)) (setf property 0))
                 (t (xclhb:change-property owner 0 window property target 8 (length text) text)))
           (let ((reply (xclhb::make-buffer 32)))
             (xclhb::write-selection-notify-event
              reply (xclhb::make-offset)
              (xclhb:make-selection-notify-event :code xclhb:+selection-notify-event+
                :time time :requestor window :selection primary :target target :property property))
             (xclhb:send-event owner 0 window 0 reply)
             (xclhb:flush owner)))))
      (funcall function (lambda () (xclhb:process-input owner) (null targets))))))

(defun xkeys (&rest keys)
  ;; No --window: xdotool injects real key events with XTEST into the focus.
  (bounded 6 "xdotool XTEST keys"
           (lambda ()
             (uiop:run-program (list* "env" (concatenate 'string "DISPLAY=" *display*)
                                      "xdotool" "key" "--delay" "0" keys)
                               :input "/dev/null" :output *standard-output* :error-output *error-output*))))

(defun delivered-bytes (file)
  (with-open-file (in file :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (when in
      (let ((data (make-array (file-length in) :element-type '(unsigned-byte 8))))
        (check (<= (length data) 4096) "Fixture received unexpectedly large terminal input")
        (read-sequence data in)
        data))))

(defun expect-bytes (label file expected &optional dispatch)
  (let ((actual nil))
    (handler-case
        (wait-for label (lambda ()
                          (when dispatch (funcall dispatch))
                          (check (uiop:process-alive-p *terminal*) "Terminal exited; see terminal logs")
                          (setf actual (delivered-bytes file))
                          (when actual
                            (check (and (<= (length actual) (length expected))
                                        (every #'= actual expected))
                                   "~A: expected ~S, got ~S" label expected actual)
                            (= (length actual) (length expected)))))
      (error (c) (error "~A (expected ~S, last child bytes ~S)" c expected actual)))))

(defun append-bytes (&rest parts)
  (apply #'concatenate '(vector (unsigned-byte 8)) parts))

(defun paste-checks (client window ready file bracketed)
  (wait-for "raw child readiness and ordered terminal mode barrier"
            (lambda () (probe-file ready)) 20)
  (xclhb:set-input-focus client 0 window 0)
  (check (= (xclhb:get-input-focus-reply-focus (xclhb:get-input-focus-sync client)) window)
         "Could not focus the mapped terminal")
  (let ((so-far (octets "")) (primary (intern-x-atom client "PRIMARY")))
    (flet ((paste (label source expected &optional fallback)
             (with-primary window source fallback
               (lambda (dispatch)
                 (xkeys "shift+Insert")
                 (setf so-far (append-bytes so-far expected))
                 (expect-bytes label file so-far dispatch)
                 (check (funcall dispatch) "Did not receive all expected PRIMARY requests")
                 ;; Check the following normal key too, so leftover paste bytes
                 ;; cannot masquerade as a successfully delivered prefix.
                 (xkeys "z")
                 (setf so-far (append-bytes so-far (octets "z")))
                 (expect-bytes label file so-far dispatch)))
             (passed "~A" label)))
      (if bracketed
          (paste "PRIMARY exact bracketed abc" (octets "abc")
                 (octets (format nil "~C[200~Aabc~C[201~A" #\Esc "~" #\Esc "~")))
          (progn
            (paste "PRIMARY plain abc" (octets "abc") (octets "abc"))
            (paste "PRIMARY UTF8_STRING non-ASCII" (octets "λ") (octets "λ"))
            (paste "PRIMARY UTF8_STRING refusal then Latin-1 STRING" #(#xE9) (octets "é") t)
            (with-primary window nil nil
              (lambda (dispatch)
                (xkeys "ctrl+shift+Insert" "z")
                (setf so-far (append-bytes so-far (octets (format nil "~C[2;6~~z" #\Esc))))
                (expect-bytes "Ctrl+Shift+Insert remains a normal key" file so-far dispatch)))
            (passed "Ctrl+Shift+Insert sends ESC[2;6~~ without requesting PRIMARY")
            (check (zerop (selection-owner client primary)) "Expected no PRIMARY owner")
            ;; Keep PRIMARY unowned through the end of the case: a new owner
            ;; could receive an in-flight fallback at the same X timestamp.
            (xkeys "shift+Insert" "z" "a")
            (setf so-far (append-bytes so-far (octets "za")))
            (expect-bytes "no-owner Shift+Insert adds no bytes before normal keys" file so-far)
            (passed "no-owner Shift+Insert produces no bytes and leaves input live"))))))

(defun run-terminal (label class)
  (let ((ready (artifact (format nil "~A.ready" label)))
        (file (artifact (format nil "~A.bytes" label)))
        (*terminal* nil))
    (with-connection (client)
      (let ((window nil) (root (root-window client)))
        ;; Redirect maps from the terminal's connection. Inspect each property
        ;; on the actual MapRequest before we permit the window to map.
        (extension-opcode client "XTEST")
        (xclhb:set-event-handler client xclhb:+map-request-event+
          (lambda (event) (setf window (xclhb:map-request-event-window event))))
        (apply #'xclhb:change-window-attributes client root #x800
               (append (make-list 11 :initial-element 0) (list #x100000 0 0 0)))
        (check (= (xclhb:get-window-attributes-reply-your-event-mask
                   (xclhb:get-window-attributes-sync client root)) #x100000)
               "Could not install SubstructureRedirect on private screen")
        (unwind-protect
             (progn
               (setf *terminal*
                     (uiop:launch-program
                      (append (list "env" (concatenate 'string "DISPLAY=" *display*)
                                    "HOSTNAME=x11-admission-invalid-hostname" "bin/common-term"
                                    "--backend" "x11" "--renderer" "cpu")
                              (when class (list "--class" class))
                              (list "--" "bash" "tests/fixtures/x11-admission-child.sh" label
                                    (namestring file) (namestring ready)))
                      :input "/dev/null" :output (artifact (format nil "~A.stdout" label))
                      :error-output (artifact (format nil "~A.stderr" label))))
               (wait-for "pre-map MapRequest"
                         (lambda ()
                           (xclhb:process-input client)
                           (check (uiop:process-alive-p *terminal*) "Launcher exited; see ~A.stderr" label)
                           window) 30)
               (bounded 6 "pre-map X11 identity"
                        (lambda () (verify-identity client window (or class "common-term"))))
               (xclhb:map-window client window)
               (wait-for "map completion"
                         (lambda () (= (xclhb:get-window-attributes-reply-map-state
                                        (xclhb:get-window-attributes-sync client window)) 2)))
               (bounded 65 "native PRIMARY cases"
                        (lambda () (paste-checks client window ready file (not (null class))))))
          (stop-process *terminal*))))))

(let ((*server* nil) (*display* nil))
  (unwind-protect
       (progn
         (setf *server* (uiop:launch-program
                         '("Xvfb" "-displayfd" "1" "-screen" "0" "1024x768x24" "-nolisten" "tcp" "-ac" "-noreset")
                         :input "/dev/null" :output :stream :error-output (artifact "xvfb.stderr")))
         (let ((number (bounded 8 "Xvfb displayfd"
                                (lambda () (read-line (uiop:process-info-output *server*) nil nil)))))
           (check (and number (plusp (length number)) (every #'digit-char-p number))
                  "Xvfb did not provide a private display number; see xvfb.stderr")
           (setf *display* (concatenate 'string ":" number)))
         (run-terminal "plain" nil)
         (run-terminal "bracketed" "ttest-admission-class")
         (passed "X11 admission"))
    (stop-process *server*)))
