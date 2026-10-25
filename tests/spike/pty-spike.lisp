;;;; S1 spike: PTY child with controlling tty, resize, and own fd 0-2 rebinding.
;;;; Run in a disposable process: sbcl --script tests/spike/pty-spike.lisp
;;;; Prints one READ-able plist per check and exits 0 only when all pass.

(require :sb-posix)

(defpackage #:lispterm-spike (:use #:cl))
(in-package #:lispterm-spike)

(defconstant +tiocswinsz+ #x5414)
(defconstant +tiocgwinsz+ #x5413)
(defconstant +tiocsctty+ #x540E)

(defun openpty ()
  "Return master and slave fds from libc openpty."
  (sb-alien:with-alien ((master sb-alien:int) (slave sb-alien:int))
    (let ((rc (sb-alien:alien-funcall
               (sb-alien:extern-alien "openpty"
                                      (function sb-alien:int (* sb-alien:int) (* sb-alien:int)
                                                (* t) (* t) (* t)))
               (sb-alien:addr master) (sb-alien:addr slave)
               nil nil nil)))
      (assert (zerop rc) () "openpty failed")
      (values master slave))))

(defun ioctl-ptr (fd request sap)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "ioctl" (function sb-alien:int sb-alien:int sb-alien:unsigned-long
                                             sb-sys:system-area-pointer))
   fd request sap))

(defun set-winsize (fd rows cols)
  (sb-alien:with-alien ((ws (array (sb-alien:unsigned 16) 4)))
    (setf (sb-alien:deref ws 0) rows (sb-alien:deref ws 1) cols
          (sb-alien:deref ws 2) 0 (sb-alien:deref ws 3) 0)
    (ioctl-ptr fd +tiocswinsz+ (sb-alien:alien-sap ws))))

(defun get-winsize (fd)
  (sb-alien:with-alien ((ws (array (sb-alien:unsigned 16) 4)))
    (ioctl-ptr fd +tiocgwinsz+ (sb-alien:alien-sap ws))
    (list (sb-alien:deref ws 0) (sb-alien:deref ws 1))))

(defun read-available (fd timeout)
  "Read octets from FD until TIMEOUT seconds pass with no data or EOF/EIO."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (buf (make-array 4096 :element-type '(unsigned-byte 8))))
    (loop
      (unless (sb-sys:wait-until-fd-usable fd :input timeout)
        (return out))
      (let ((n (sb-sys:with-pinned-objects (buf)
                 (sb-unix:unix-read fd (sb-sys:vector-sap buf) (length buf)))))
        (if (and n (plusp n))
            (loop for i below n do (vector-push-extend (aref buf i) out))
            (return out))))))

(defun octets->string (octets)
  (sb-ext:octets-to-string (coerce octets '(simple-array (unsigned-byte 8) (*)))
                           :external-format :utf-8))

(defun report (name ok &rest data)
  (format t "~S~%" (list* :check name :ok (and ok t) data))
  (finish-output)
  ok)

;;; Check 1: SB-EXT:RUN-PROGRAM :PTY T only calls setpgrp, so the child has
;;; no controlling tty (observed CTTY=? and no SIGWINCH).  Instead spawn with
;;; glibc posix_spawn + POSIX_SPAWN_SETSID and open the slave by name in the
;;; file actions: a session leader opening a tty without O_NOCTTY acquires it.
(defconstant +posix-spawn-setsigdef+ #x04)
(defconstant +posix-spawn-setsigmask+ #x08)
(defconstant +posix-spawn-setsid+ #x80)

(defun libc (name)
  (sb-sys:find-foreign-symbol-address name))

(defmacro c-call (name result &rest typed-args)
  "Call libc NAME; TYPED-ARGS alternate alien type and value."
  (let ((types (loop for (type) on typed-args by #'cddr collect type))
        (values (loop for (nil value) on typed-args by #'cddr collect value)))
    `(sb-alien:alien-funcall
      (sb-alien:extern-alien ,name (function ,result ,@types))
      ,@values)))

(defun ptsname (master)
  (c-call "ptsname" sb-alien:c-string sb-alien:int master))

(defun spawn-on-pty (slave-path argv)
  "Spawn ARGV (list of strings, absolute program first) as a session leader
   whose fds 0-2 and controlling tty are SLAVE-PATH.  Return the pid."
  (let ((actions (sb-alien:make-alien (sb-alien:unsigned 8) 512))
        (attr (sb-alien:make-alien (sb-alien:unsigned 8) 512))
        (sigset (sb-alien:make-alien (sb-alien:unsigned 8) 256))
        (cargv (sb-alien:make-alien sb-alien:c-string (1+ (length argv))))
        (pid (sb-alien:make-alien sb-alien:int)))
    (unwind-protect
         (progn
           (loop for arg in argv for i from 0
                 do (setf (sb-alien:deref cargv i) arg))
           (setf (sb-alien:deref cargv (length argv)) nil)
           (assert (zerop (c-call "posix_spawn_file_actions_init" sb-alien:int (* t) actions)))
           (assert (zerop (c-call "posix_spawn_file_actions_addopen" sb-alien:int
                                  (* t) actions sb-alien:int 0 sb-alien:c-string slave-path
                                  sb-alien:int sb-posix:o-rdwr sb-alien:unsigned 0)))
           (assert (zerop (c-call "posix_spawn_file_actions_adddup2" sb-alien:int
                                  (* t) actions sb-alien:int 0 sb-alien:int 1)))
           (assert (zerop (c-call "posix_spawn_file_actions_adddup2" sb-alien:int
                                  (* t) actions sb-alien:int 0 sb-alien:int 2)))
           (assert (zerop (c-call "posix_spawnattr_init" sb-alien:int (* t) attr)))
           (assert (zerop (c-call "sigemptyset" sb-alien:int (* t) sigset)))
           (assert (zerop (c-call "posix_spawnattr_setsigmask" sb-alien:int
                                  (* t) attr (* t) sigset)))
           (assert (zerop (c-call "sigfillset" sb-alien:int (* t) sigset)))
           (assert (zerop (c-call "posix_spawnattr_setsigdefault" sb-alien:int
                                  (* t) attr (* t) sigset)))
           (assert (zerop (c-call "posix_spawnattr_setflags" sb-alien:int (* t) attr
                                  sb-alien:short (logior +posix-spawn-setsid+
                                                         +posix-spawn-setsigmask+
                                                         +posix-spawn-setsigdef+))))
           (let ((rc (c-call "posix_spawn" sb-alien:int
                             (* sb-alien:int) pid sb-alien:c-string (first argv)
                             (* t) actions (* t) attr
                             (* sb-alien:c-string) cargv
                             (* t) (sb-alien:extern-alien "environ" (* t)))))
             (assert (zerop rc) () "posix_spawn failed: ~D" rc)
             (sb-alien:deref pid)))
      (c-call "posix_spawn_file_actions_destroy" sb-alien:int (* t) actions)
      (c-call "posix_spawnattr_destroy" sb-alien:int (* t) attr)
      (mapc #'sb-alien:free-alien (list actions attr sigset cargv pid)))))

(defun write-octets (fd string)
  (let ((octets (sb-ext:string-to-octets string :external-format :utf-8)))
    (sb-sys:with-pinned-objects (octets)
      (sb-unix:unix-write fd octets 0 (length octets)))))

(defun check-child-pty ()
  (multiple-value-bind (master slave) (openpty)
    (let* ((path (ptsname master))
           (script "trap 'echo WINCH=$(stty size)' WINCH; [ -t 0 ] && echo ISATTY; echo CTTY=$(ps -o tty= -p $$); echo SIZE=$(stty size); read line; echo GOT=$line; exit 7"))
      (set-winsize master 24 80)
      (let ((pid (spawn-on-pty path (list "/bin/sh" "-c" script))))
        ;; The parent keeps no slave fd, so EIO on the master signals hangup.
        (sb-posix:close slave)
        (let ((first (octets->string (read-available master 1.0))))
          (set-winsize master 33 77)
          (let ((second (octets->string (read-available master 1.0))))
            (write-octets master (format nil "hello~C" #\Return))
            (let ((third (octets->string (read-available master 1.0))))
              (multiple-value-bind (waited status) (sb-posix:waitpid pid 0)
                (declare (ignore waited))
                (let ((all (concatenate 'string first second third))
                      (exit (and (sb-posix:wifexited status) (sb-posix:wexitstatus status))))
                  (sb-posix:close master)
                  (report :child-pty
                          (and (search "ISATTY" all)
                               (not (search "CTTY=?" all))
                               (search "WINCH=33 77" all)
                               (search "GOT=hello" all)
                               (eql 7 exit))
                          :slave path :exit exit :output all))))))))))

;;; Check 2: this process rebinds its own fds 0-2 onto a fresh slave,
;;; acquires it as controlling tty, and sees a real terminal.
(defun check-own-fds ()
  (multiple-value-bind (master slave) (openpty)
    (set-winsize master 40 120)
    (let ((setsid (sb-posix:setsid)))
      (declare (ignorable setsid))
      (ioctl-ptr slave +tiocsctty+ (sb-sys:int-sap 0))
      (dolist (fd '(0 1 2)) (sb-posix:dup2 slave fd))
      (sb-posix:close slave)
      (let* ((in (sb-sys:make-fd-stream 0 :input t :buffering :full
                                          :external-format :utf-8))
             (out (sb-sys:make-fd-stream 1 :output t :buffering :line
                                           :external-format :utf-8))
             (termios-ok (handler-case (progn (sb-posix:tcgetattr 0) t)
                           (error () nil)))
             (interactive (interactive-stream-p in))
             (isatty (= 1 (sb-unix:unix-isatty 0)))
             (size (get-winsize 0)))
        (format out "SPIKE-OUT ~A~%" size)
        (finish-output out)
        (let ((echoed (octets->string (read-available master 0.5))))
          ;; Restore reporting to a pipe-free channel: write results to the master side reader.
          (let ((ok (and termios-ok interactive isatty (equal size '(40 120))
                         (search "SPIKE-OUT (40 120)" echoed))))
            (with-open-file (log (second sb-ext:*posix-argv*) :direction :output
                                 :if-exists :append :if-does-not-exist :create)
              (format log "~S~%" (list :check :own-fds :ok (and ok t)
                                        :tcgetattr termios-ok :interactive-stream-p interactive
                                        :isatty isatty :winsize size :master-read echoed)))
            ok))))))

(let ((ok1 (check-child-pty)))
  (let ((ok2 (if (second sb-ext:*posix-argv*)
                 (check-own-fds)
                 (report :own-fds nil :reason "pass a log path argument"))))
    (sb-ext:exit :code (if (and ok1 ok2) 0 1) :abort t)))
