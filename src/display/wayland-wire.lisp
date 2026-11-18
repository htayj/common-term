;;;; Linux/x86-64 Wayland transport. No libwayland thread or callback trampoline.
(defpackage #:lispterm.display.wayland
  (:nicknames #:common-term.display.wayland)
  (:use #:cl)
  (:export #:wire-connect #:wire-close #:wire-send #:wire-poll #:wire-dispatch
           #:wayland-state #:wayland-state-configured-p))
(in-package #:lispterm.display.wayland)

(cffi:defcfun ("socket" %socket) :int (domain :int) (type :int) (protocol :int))
(cffi:defcfun ("connect" %connect) :int (fd :int) (address :pointer) (length :unsigned-int))
(cffi:defcfun ("close" %close) :int (fd :int))
(cffi:defcfun ("sendmsg" %sendmsg) :long (fd :int) (message :pointer) (flags :int))
(cffi:defcfun ("recvmsg" %recvmsg) :long (fd :int) (message :pointer) (flags :int))
(cffi:defcfun ("poll" %poll) :int (fds :pointer) (count :unsigned-long) (timeout :int))
(cffi:defcfun ("__errno_location" %errno-location) :pointer)
(defun errno () (cffi:mem-ref (%errno-location) :int))

(define-condition wayland-transport-error (error)
  ((operation :initarg :operation :reader transport-operation)
   (errno :initarg :errno :initform nil :reader transport-errno))
  (:report (lambda (c s) (format s "Wayland ~A failed~@[ (errno ~D)~]"
                                 (transport-operation c) (transport-errno c)))))
(defun wire-error (operation &optional (number (errno)))
  (error 'wayland-transport-error :operation operation :errno number))
(defstruct (wire (:constructor %make-wire))
  (fd -1 :type integer)
  (bytes (make-array 0 :element-type '(unsigned-byte 8)))
  (fds nil) (next-id 2) (handlers (make-hash-table)) (interfaces (make-hash-table)))
(defun zero-memory (pointer size)
  (dotimes (i size) (setf (cffi:mem-aref pointer :uint8 i) 0)) pointer)
(defun wire-connect (&optional name)
  "Connect only to WAYLAND_DISPLAY or the supplied native socket. No X11 access."
  #-(and linux x86-64) (error "Native Wayland wire ABI requires Linux x86-64")
  (let* ((name (or name (uiop:getenv "WAYLAND_DISPLAY") "wayland-0"))
         (path (if (and (> (length name) 0) (char= (char name 0) #\/)) name
                   (format nil "~A/~A" (or (uiop:getenv "XDG_RUNTIME_DIR")
                                           (error "XDG_RUNTIME_DIR is unset")) name)))
         (bytes (babel:string-to-octets path :encoding :utf-8))
         (fd (progn
               (when (> (length bytes) 107) (error "Wayland socket path exceeds sockaddr_un"))
               (%socket 1 #x80801 0))))
    (when (minusp fd) (wire-error "socket"))
    (handler-case
        (cffi:with-foreign-object (address :uint8 110)
          (zero-memory address 110)
          (setf (cffi:mem-ref address :uint16) 1)
          (dotimes (i (length bytes))
            (setf (cffi:mem-aref address :uint8 (+ 2 i)) (aref bytes i)))
          (when (minusp (%connect fd address (+ 3 (length bytes)))) (wire-error "connect"))
          (%make-wire :fd fd))
      (error (c) (%close fd) (error c)))))
(defun wire-close (wire)
  (when (>= (wire-fd wire) 0) (%close (wire-fd wire)) (setf (wire-fd wire) -1))
  (mapc #'%close (wire-fds wire)) (setf (wire-fds wire) nil)
  nil)
(defun u32-bytes (value)
  (let ((a (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4 a) (setf (aref a i) (ldb (byte 8 (* 8 i)) value)))))
(defun bytes-u32 (bytes offset)
  (loop for i below 4 sum (ash (aref bytes (+ offset i)) (* 8 i))))
(defun signed32 (n) (if (logbitp 31 n) (- n #x100000000) n))
(defun wl-string (string)
  (let* ((s (babel:string-to-octets string :encoding :utf-8))
         (length (1+ (length s))) (padded (* 4 (ceiling length 4)))
         (out (make-array (+ 4 padded) :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace out (u32-bytes length)) (replace out s :start1 4) out))
(defun wire-id (wire interface handler)
  (let ((id (wire-next-id wire)))
    (when (> id #xfeffffff) (wire-error "object ID space exhausted" nil))
    (incf (wire-next-id wire))
    (setf (gethash id (wire-interfaces wire)) interface
          (gethash id (wire-handlers wire)) handler) id))
;; Opt-in diagnostics. The hook sees object IDs, opcodes and raw arguments;
;; it alone decides what is safe to record.
(defvar *wire-trace* nil
  "NIL, or a function of (KIND WIRE OBJECT OPCODE DATA) called for :SEND and :EVENT.")
(defvar *wire-dispatch-depth* 0)
(defun wire-send (wire object opcode &rest arguments)
  "Arguments are 32-bit words, encoded byte vectors, or (:FD integer).
SCM_RIGHTS does not occupy bytes in the protocol body."
  (when *wire-trace* (funcall *wire-trace* :send wire object opcode arguments))
  (let* ((fds (loop for a in arguments when (consp a) collect (second a)))
         (body (apply #'concatenate '(vector (unsigned-byte 8))
                      (loop for a in arguments unless (consp a)
                            collect (if (integerp a) (u32-bytes a) a))))
         (size (+ 8 (length body)))
         (bytes (concatenate '(vector (unsigned-byte 8)) (u32-bytes object)
                             (u32-bytes (logior (ash size 16) opcode)) body)))
    (when (> size 65535) (error "Wayland request too large"))
    (cffi:with-pointer-to-vector-data (data bytes)
      (cffi:with-foreign-objects ((iov :uint8 16) (msg :uint8 56)
                                (control :uint8 (+ 16 (* 8 (max 1 (length fds))))))
        (zero-memory msg 56) (zero-memory control (+ 16 (* 8 (max 1 (length fds)))))
        (setf (cffi:mem-ref iov :pointer) data
              (cffi:mem-ref iov :unsigned-long 8) size
              (cffi:mem-ref msg :pointer 16) iov
              (cffi:mem-ref msg :unsigned-long 24) 1)
        (when fds
          (let* ((length (+ 16 (* 4 (length fds)))) (space (* 8 (ceiling length 8))))
            (setf (cffi:mem-ref msg :pointer 32) control
                  (cffi:mem-ref msg :unsigned-long 40) space
                  (cffi:mem-ref control :unsigned-long) length
                  (cffi:mem-ref control :int 8) 1 ; SOL_SOCKET
                  (cffi:mem-ref control :int 12) 1) ; SCM_RIGHTS
            (loop for fd in fds for i from 0 do
              (setf (cffi:mem-ref control :int (+ 16 (* i 4))) fd))))
        ;; Nonblocking send, no SIGPIPE; a partial request makes this connection unusable.
        (unless (= (%sendmsg (wire-fd wire) msg (logior #x40 #x4000)) size)
          (wire-error "sendmsg/partial write")))))
  nil)
(defun wire-readable-p (wire timeout)
  (cffi:with-foreign-object (p :uint8 8)
    (zero-memory p 8)
    (setf (cffi:mem-ref p :int) (wire-fd wire) (cffi:mem-ref p :short 4) 1)
    (let ((result (%poll p 1 (max 0 (ceiling (* 1000 (or timeout 0)))))))
      (cond ((minusp result) (unless (= (errno) 4) (wire-error "poll")) nil)
            ((zerop result) nil)
            (t t)))))
(defun wire-poll (wire &optional (timeout 0))
  "Receive one stream chunk and all accompanying descriptors with CLOEXEC."
  (when (wire-readable-p wire timeout)
    (cffi:with-foreign-objects ((data :uint8 65536) (iov :uint8 16)
                              (msg :uint8 56) (control :uint8 4096))
      (zero-memory msg 56)
      (setf (cffi:mem-ref iov :pointer) data
            (cffi:mem-ref iov :unsigned-long 8) 65536
            (cffi:mem-ref msg :pointer 16) iov
            (cffi:mem-ref msg :unsigned-long 24) 1
            (cffi:mem-ref msg :pointer 32) control
            (cffi:mem-ref msg :unsigned-long 40) 4096)
      (let ((n (%recvmsg (wire-fd wire) msg (logior #x40 #x40000000))))
        (when (minusp n)
          (if (member (errno) '(4 11)) (return-from wire-poll nil) (wire-error "recvmsg")))
        (when (zerop n) (wire-error "compositor disconnected" nil))
        ;; Parse descriptors before checking truncation, so cleanup owns every received FD.
        (loop with used = (cffi:mem-ref msg :unsigned-long 40)
              for offset = 0 then (+ offset (* 8 (ceiling length 8)))
              while (<= (+ offset 16) used)
              for length = (cffi:mem-ref control :unsigned-long offset)
              do (when (or (< length 16) (> (+ offset length) used))
                   (wire-error "malformed ancillary header" nil))
                 (when (and (= (cffi:mem-ref control :int (+ offset 8)) 1)
                            (= (cffi:mem-ref control :int (+ offset 12)) 1))
                   (loop for pos from (+ offset 16) below (+ offset length) by 4
                         do (setf (wire-fds wire)
                                  (nconc (wire-fds wire)
                                         (list (cffi:mem-ref control :int pos)))))))
        (when (logtest #x28 (cffi:mem-ref msg :int 48))
          (wire-error "truncated recvmsg" nil))
        (let ((chunk (make-array n :element-type '(unsigned-byte 8))))
          (dotimes (i n) (setf (aref chunk i) (cffi:mem-aref data :uint8 i)))
          (setf (wire-bytes wire) (concatenate '(vector (unsigned-byte 8))
                                              (wire-bytes wire) chunk)))
        n))))
(defun wire-dispatch (wire)
  (loop while (>= (length (wire-bytes wire)) 8)
        for bytes = (wire-bytes wire)
        for object = (bytes-u32 bytes 0)
        for word = (bytes-u32 bytes 4)
        for size = (ash word -16) for opcode = (logand word #xffff)
        do (unless (and (>= size 8) (zerop (mod size 4)))
             (wire-error "invalid message size" nil))
           (when (< (length bytes) size) (return))
           (let ((handler (gethash object (wire-handlers wire)))
                 (payload (subseq bytes 8 size)))
             (when *wire-trace* (funcall *wire-trace* :event wire object opcode payload))
             (unless handler (wire-error (format nil "unknown object ~D" object) nil))
             ;; Remove before dispatch. A handler may recursively pump during initialization.
             (setf (wire-bytes wire) (subseq bytes size))
             (let ((*wire-dispatch-depth* (1+ *wire-dispatch-depth*)))
               (funcall handler object opcode payload)))))
(defun wire-fd-pop (wire)
  (or (pop (wire-fds wire)) (wire-error "missing received file descriptor" nil)))
(defun payload-string (payload offset)
  (let ((length (bytes-u32 payload offset)))
    (when (or (zerop length) (> (+ offset 4 length) (length payload)))
      (wire-error "invalid protocol string" nil))
    (values (babel:octets-to-string payload :encoding :utf-8
                                   :start (+ offset 4) :end (+ offset 3 length))
            (+ offset 4 (* 4 (ceiling length 4))))))
