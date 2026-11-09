;;;; Rooted comparison-record fixtures for the screen, double-size and
;;;; character-set contracts. Native receipts carry LOGICAL dimensions; the
;;;; record, identify output and decoded RGBA carry PHYSICAL dimensions.
;;;; PNG octets are synthetic, so these fixtures are not pixel evidence.
(defpackage :vttest-physical-contract-tests (:use :cl) (:export :run-tests))
(in-package :vttest-physical-contract-tests)

(defvar *parent* nil "Retained directory holding this run's fixture roots.")
(defparameter +font+
  '(:path "/nix/store/9qvd27vg12177prxcl778rdhxh4lk2mi-terminus-font-4.49.1/share/fonts/terminus/ter-u16n.otb"
    :sha256 "0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca"))
(defparameter +decoder+
  '(:path "/nix/store/h74lg8wfwndnf5glr171kmcshlffi7l2-imagemagick-7.1.2-29/bin/magick"
    :sha256 "054c52b583c303bca2aae9fadcfd9e09fbc946f83e6621b39f22650d28d3027e"))
;;; (AREA CONTRACT-PACKAGE WIDE-PAUSES ACCEPTED-PAUSES)
(defparameter +areas+
  '((:screen :vttest-screen-native-contract (3 5) (1 3 5))
    (:double :vttest-double-size-native-contract (3 4) (1 3 4))
    (:charsets :vttest-charsets-native-contract () (1))))
(defparameter +bracket+ '("before.sexp" "ready.sexp" "pre.sexp" "final.sexp" "native.sexp"))

(defun area-fn (area name)
  (symbol-function (find-symbol name (second (assoc area +areas+)))))
(defun logical-width (area n)
  (if (member n (third (assoc area +areas+))) 1056 640))

(defun status (columns frames generation)
  "A settled Wayland status for the 24-row grid with logical 8x16 cells."
  (list :id 1 :child-pid 2 :backend :wayland :rows 24 :columns columns
        :pty-rows 24 :pty-columns columns
        :geometry (list :phase :settled :request-id 1 :display-epoch 1 :epoch 1
                        :actual-width (* 8 columns) :actual-height 384
                        :cell-width 8 :cell-height 16 :presentation 2
                        :target-presentation 2 :rows 24 :columns columns :window-id 7
                        :frame-pending-p nil :commit-generation generation
                        :frame-generation generation)
        :frames frames :blink-phase t :blink-visible nil :bytes-received 100
        :input-pending 0 :error-count 0 :process-state :running :transport-state :open))

(defun native-receipt (n logical-width scale)
  (list :version 3 :backend :wayland :pause n :window-id 7 :surface-id 8
        :width logical-width :height 384 :scale scale :owned t :custody t
        :capture-count 1 :capture-exit 0
        :png (format nil "pause-~2,'0d-actual.png" n) :ack-output 100))

(defun put-text (path text)
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :error :if-does-not-exist :create)
    (write-sequence (sb-ext:string-to-octets text :external-format :utf-8) s)))
(defun put-sparse (path length)
  "Create a LENGTH-octet zero file; only its length is decoded evidence here."
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :error :if-does-not-exist :create)
    (file-position s (1- length))
    (write-byte 0 s)))

(defun legacy (r)
  "Version-1 shape: no :SCALE and no physical size inside the comparison."
  (let ((c (getf r :comparison)))
    (dolist (k '(:scale :width :height :last-difference)) (remf c k))
    (remf r :scale)
    (setf (getf r :version) 1 (getf r :comparison) c)
    r))

(defun build (area n scale &key (version 2) (record #'identity) (comparison #'identity)
                                (native #'identity) (rgba-delta 0) identify)
  "Exclusively write one complete rooted pause; return its check and root."
  (let* ((root (concatenate 'string (sb-posix:mkdtemp (concatenate 'string *parent* "case-XXXXXX")) "/"))
         (lw (logical-width area n)) (w (* scale lw)) (h (* scale 384))
         (stem (format nil "recorder/pause-~2,'0d-" n))
         (ready (status (/ lw 8) 11 5))
         (nat (funcall native (native-receipt n lw scale))))
    (ensure-directories-exist (concatenate 'string root "recorder/"))
    (labels ((path (suffix) (concatenate 'string root stem suffix))
             (ref (suffix)
               (funcall (area-fn area "REFERENCE") root (concatenate 'string stem suffix)))
             (data (suffix value)
               (funcall (area-fn area "WRITE-DATA") (path suffix) (copy-tree value)))
             (evidence (action argv stdout)
               (data (format nil "~a.argv.sexp" action) argv)
               (put-text (path (format nil "~a.stdout" action)) stdout)
               (put-text (path (format nil "~a.stderr" action)) "")
               (data (format nil "~a.exit" action) 0)
               (loop for suffix in '(".argv.sexp" ".stdout" ".stderr" ".exit")
                     collect (ref (concatenate 'string action suffix)))))
      (put-text (path "actual.png") "synthetic png octets")
      (put-sparse (path "actual.rgba") (+ (* w h 4) rgba-delta))
      (loop for value in (list (status (/ lw 8) 10 4) ready ready ready nat)
            for suffix in +bracket+ do (data suffix value))
      (data "request.sexp" (list :status ready :pause n :target 100 :ack-output 100 :surface-id 8))
      (let* ((png (funcall (area-fn area "LOCAL-PATH") root (concatenate 'string stem "actual.png")))
             (magick (getf +decoder+ :path))
             (facts (list :pass t :scale scale :width w :height h :compared-pixels (* w h)
                          :different-pixels 0 :different-cells nil :first-difference nil
                          :last-difference nil :tolerance 0 :masked-pixels 0))
             (r (list :version 2 :pause n :png (ref "actual.png") :phase t :scale scale
                      :width w :height h :font +font+ :decoder +decoder+
                      :bracket (mapcar #'ref +bracket+)
                      :identify (evidence "identify" (list magick "identify" "-format" "%w %h" png)
                                          (or identify (format nil "~d ~d" w h)))
                      :decode (evidence "decode" (list magick png "-depth" "8"
                                                       (concatenate 'string "rgba:" root stem "actual.rgba"))
                                        "")
                      :rgba (ref "actual.rgba") :decoded-length (* w h 4) :opaque t
                      :comparison facts)))
        (when (eql version 1) (setf r (legacy r)))
        (setf (getf r :comparison) (funcall comparison (getf r :comparison)))
        (data "comparison.sexp" (funcall record r))
        (values (list :pause n :native nat
                      :comparison (list :path (concatenate 'string stem "comparison.sexp")))
                root)))))
