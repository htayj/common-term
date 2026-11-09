;;;; Prove this run's own primary Device Attributes reply preceded its keys.
;;;; Pure: no font, pixel, processed-row or native timing evidence is used.
(in-package #:vttest-keyboard-driver)

(defparameter +da-query+ (format nil "~c[c" (code-char 27))
  "Primary DA request as written by vttest.")
(defparameter +da-reply+ (format nil "~c[?1;2c" (code-char 27))
  "Terminal answer. Octet counts are derived from this literal.")

(defun own-da-reply-p (label events output)
  "Return T when EVENTS and OUTPUT prove LABEL's own DA exchange, otherwise
NIL and a keyword naming the first failed requirement.

LABEL names a keyboard case. EVENTS is the plist list EVENTS-OF returns,
using the recorder's :KIND :OUT :IN :BYTES :END :SOURCE fields; :OUT and :IN
count octets recorded before the event. OUTPUT is the complete transcript,
one character per octet.

Requirements, checked in this order:
- every event's :OUT and :IN equal the octets of all earlier :WIRE-OUT and
  :WIRE-IN events, and each wire event's :END extends its counter by its
  bytes (:PROGRESS, :OUTPUT-LINK, :INPUT-LINK);
- :WIRE-OUT bytes are OUTPUT's octets at their offsets and together cover
  exactly OUTPUT (:OUTPUT-LINK, :OUTPUT-LENGTH);
- before the first :KEY-BEGIN, with :ENQUEUE replayed as a FIFO, each
  :WIRE-IN is a prefix of the head entry of the same :SOURCE (:ENQUEUE, :FIFO);
- OUTPUT contains +DA-QUERY+ and EVENTS contain a :KEY-BEGIN
  (:NO-QUERY, :NO-KEY-BEGIN);
- exactly one :ENQUEUE has :SOURCE :REPLY (:REPLY-COUNT), its bytes equal
  +DA-REPLY+ (:REPLY-BYTES), it precedes the first :KEY-BEGIN (:LATE-REPLY)
  and its :OUT covers the query's last octet (:REPLY-BEFORE-QUERY);
- :REPLY-sourced :WIRE-IN events consume all of its octets before that
  :KEY-BEGIN (:UNWIRED-REPLY)."
  (flet ((proper-length (x)
           ;; Element count of a proper noncircular list X, else NIL. LIST-LENGTH
           ;; returns NIL for a circular list and signals on a dotted one.
           (and (listp x) (handler-case (list-length x) (type-error () nil)))))
    (unless (and (case-of label) (stringp output) (proper-length events)
                 (every (lambda (e) (let ((n (proper-length e))) (and n (evenp n)))) events))
      (return-from own-da-reply-p (values nil :arguments))))
  (let ((first-key (position :key-begin events :key (lambda (e) (getf e :kind))))
        (out 0) (in 0) (fifo nil) (wired 0))
    (flet ((fail (reason) (return-from own-da-reply-p (values nil reason))))
      (loop for e in events for i from 0
            for kind = (getf e :kind) for bytes = (getf e :bytes)
            for end = (getf e :end) for before-key = (or (null first-key) (< i first-key))
            do (unless (and (eql (getf e :out) out) (eql (getf e :in) in)) (fail :progress))
               (case kind
                 (:wire-out
                  (unless (and (stringp bytes) (plusp (length bytes)) (null (getf e :source))
                               (eql end (+ out (length bytes))) (<= end (length output))
                               (string= bytes output :start2 out :end2 end))
                    (fail :output-link))
                  (setf out end))
                 (:wire-in
                  (unless (and (stringp bytes) (plusp (length bytes))
                               (eql end (+ in (length bytes))))
                    (fail :input-link))
                  (setf in end)
                  (when before-key
                    (let ((head (first fifo)) (n (length bytes)))
                      (unless (and head (eq (first head) (getf e :source))
                                   (<= n (length (second head)))
                                   (string= bytes (second head) :end2 n))
                        (fail :fifo))
                      (setf (second head) (subseq (second head) n))
                      (when (eq (first head) :reply) (incf wired n))
                      (when (zerop (length (second head))) (pop fifo)))))
                 (:enqueue
                  (when before-key
                    (unless (and (member (getf e :source) '(:input :reply))
                                 (stringp bytes) (plusp (length bytes)))
                      (fail :enqueue))
                    (setf fifo (append fifo (list (list (getf e :source) bytes))))))))
      (unless (= out (length output)) (fail :output-length))
      (let ((query (search +da-query+ output))
            (replies (remove-if-not (lambda (e) (and (eq (getf e :kind) :enqueue)
                                                     (eq (getf e :source) :reply)))
                                    events)))
        (unless query (fail :no-query))
        (unless first-key (fail :no-key-begin))
        (unless (= 1 (length replies)) (fail :reply-count))
        (let ((reply (first replies)))
          (unless (equal (getf reply :bytes) +da-reply+) (fail :reply-bytes))
          (unless (< (position reply events) first-key) (fail :late-reply))
          (unless (>= (getf reply :out) (+ query (length +da-query+)))
            (fail :reply-before-query)))
        (unless (= wired (length +da-reply+)) (fail :unwired-reply))
        (values t nil)))))
