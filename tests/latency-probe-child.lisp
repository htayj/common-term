;;;; Raw echo probe. Answers input byte number N with token N and logs the
;;;; monotonic time each byte was read and each token write completed.
;;;; sbcl --script tests/latency-probe-child.lisp NONCE SLOTS COUNT LOG
(require :sb-posix)
(load (merge-pathnames "latency-tokens.lisp" *load-truename*))
(defpackage #:lispterm.latency.probe (:use #:cl #:lispterm.latency.tokens))
(in-package #:lispterm.latency.probe)

(declaim (inline monotonic-ns))
(defun monotonic-ns ()
  (multiple-value-bind (seconds nanoseconds)
      (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* seconds 1000000000) nanoseconds)))

(defun enter-raw-mode (fd)
  (let ((termios (sb-posix:tcgetattr fd)))
    (setf (sb-posix:termios-lflag termios)
          (logandc2 (sb-posix:termios-lflag termios)
                    (logior sb-posix:icanon sb-posix:echo sb-posix:isig sb-posix:iexten))
          (sb-posix:termios-iflag termios)
          (logandc2 (sb-posix:termios-iflag termios) (logior sb-posix:icrnl sb-posix:ixon))
          (sb-posix:termios-oflag termios)
          (logandc2 (sb-posix:termios-oflag termios) sb-posix:opost)
          (aref (sb-posix:termios-cc termios) sb-posix:vmin) 1
          (aref (sb-posix:termios-cc termios) sb-posix:vtime) 0)
    (sb-posix:tcsetattr fd sb-posix:tcsanow termios)))

(defun write-octets (octets)
  "Write all of OCTETS to standard output; exit when the terminal is gone."
  (loop with offset = 0
        while (< offset (length octets))
        do (multiple-value-bind (written errno)
               (sb-unix:unix-write 1 octets offset (- (length octets) offset))
             (cond (written (incf offset written))
                   ((member errno (list sb-posix:eintr sb-posix:eagain)))
                   (t (sb-ext:exit :code 3 :abort t))))))

(defun read-byte-blocking (sap)
  "True once one byte is stored at SAP; NIL at end of input."
  (loop
    (multiple-value-bind (count errno) (sb-unix:unix-read 0 sap 1)
      (cond ((eql count 1) (return t))
            ((and (null count) (= errno sb-posix:eintr)))
            (t (return nil))))))

(defun main (nonce slots count log)
  (let ((tokens (make-array count))
        (records (make-array count :initial-element nil))
        (completed 0))
    ;; Nothing is formatted or allocated between a read and its answer.
    (dotimes (index count) (setf (aref tokens index) (token-bytes nonce index slots)))
    (enter-raw-mode 0)
    (write-octets (ready-bytes nonce))
    (sb-alien:with-alien ((buffer (array sb-alien:unsigned-char 1)))
      (loop while (and (< completed count) (read-byte-blocking (sb-alien:alien-sap buffer)))
            do (let* ((read-at (monotonic-ns))
                      (write-start (monotonic-ns)))
                 (write-octets (aref tokens completed))
                 (let ((write-end (monotonic-ns)))
                   (setf (aref records completed)
                         (list completed (sb-alien:deref buffer 0) read-at write-end
                               write-start nonce)))
                 (incf completed))))
    (with-open-file (out log :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (print (list :probe :nonce nonce :slots slots :count count :completed completed) out)
        (dotimes (index completed) (print (aref records index) out))
        (terpri out)))))

(destructuring-bind (nonce slots count log) (last sb-ext:*posix-argv* 4)
  (main nonce (parse-integer slots) (parse-integer count) log))
