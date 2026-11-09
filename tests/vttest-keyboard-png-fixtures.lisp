;;;; PNG validation fixtures: fresh owned directories, synthetic PNG header
;;;; octets, independent opaque 640x384 RGBA8 rasters, a never-run stand-in
;;;; executable and a controlled *PIXEL-TOOL* hook. The header is only enough
;;;; for PNG-HEAD; it proves nothing about decoder validity. Runs no tools.
;;;; Load after vttest-keyboard-driver-tests.lisp and vttest-keyboard-png.lisp
;;;; (with their area, cursor driver/oracle and keyboard artifact dependencies).
(in-package :vttest-keyboard-driver-tests)

(dolist (f '(k::capture-pixels k::png-snapshot k::png-head temp-root))
  (unless (fboundp f) (error "PNG fixtures need ~s loaded first" f)))

(defun png-root ()
  "Fresh owned root (ending in /) with empty run/ and scratch/ subdirectories."
  (let ((root (temp-root)))
    (dolist (sub '("run/" "scratch/") root)
      (sb-posix:mkdir (c::cat root sub) #o700))))

(defun png-octets (list)
  (coerce list '(vector (unsigned-byte 8))))

(defun png-header (&key (width 640) (height 384) (depth 8) (colour 6))
  "Signature and an IHDR chunk with a zero CRC: synthetic, not a decodable PNG."
  (flet ((u32 (n) (loop for s from 24 downto 0 by 8 collect (ldb (byte 8 s) n))))
    (png-octets (append '(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82)
                        (u32 width) (u32 height) (list depth colour 0 0 0) (u32 0)))))

(defun opaque-rgba (&optional (blue 0))
  "Fresh opaque 640x384 RGBA8 raster: red = x mod 256, green = y mod 256."
  (let ((v (make-array (* 640 384 4) :element-type '(unsigned-byte 8))))
    (dotimes (y 384 v)
      (dotimes (x 640)
        (replace v (list (mod x 256) (mod y 256) blue 255) :start1 (* 4 (+ x (* 640 y))))))))

(defun put-octets (path octets)
  "Write OCTETS to PATH, replacing it; return PATH."
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence octets s))
  path)

(defun write-png (dir name &optional (octets (png-header)))
  (put-octets (c::cat dir name ".png") octets))

(defun png-sha (path)
  "Area SHA-256 of PATH read through the no-follow descriptor."
  (nth-value 1 (k::png-snapshot path)))

(defun raw-octets (path) (vttest-cursor-driver::bytes path))

(defun fixture-magick (root &key (mode #o755) link)
  "Create ROOT bin/magick, a stand-in never executed (hooks replace it). With
LINK also create bin/magick-link pointing at it and return the link path."
  (let* ((bin (ensure-directories-exist (c::cat root "bin/")))
         (exe (put-octets (c::cat bin "magick")
                          (png-octets (map 'list #'char-code
                                           (format nil "#!/bin/sh~%exit 97~%"))))))
    (sb-posix:chmod exe mode)
    (if link
        (let ((l (c::cat bin "magick-link"))) (sb-posix:symlink exe l) l)
        exe)))

(defvar *png-argv* '() "Full argv lists the PNG hook received, newest first.")

(defun png-hook (&key (identify '("640 384" 0 "")) (decode '("" 0 "")) raw after-decode)
  "A *PIXEL-TOOL* stand-in. IDENTIFY and DECODE are (stdout exit stderr) for the
call whose second argv element is \"identify\" or not. On a decode, RAW octets
(when given) go to the path after the final argument's rgba: prefix, then
AFTER-DECODE is called with the argv, e.g. to mutate the PNG."
  (lambda (argv)
    (push argv *png-argv*)
    (if (equal (second argv) "identify")
        (values-list identify)
        (let ((target (car (last argv))))
          (when (and raw (eql 0 (search "rgba:" target)))
            (put-octets (subseq target 5) raw))
          (when after-decode (funcall after-decode argv))
          (values-list decode)))))

(defun run-png (root name expected &key (hook (png-hook)) (magick (fixture-magick root)))
  "CAPTURE-PIXELS on ROOT run/NAME with ROOT scratch/, HOOK and MAGICK bound and
the counters scoped. Plist :STATUS :REASONS :SHA :RAW :TOOLS :COMPARISONS and
:ARGV oldest first; an escaped error becomes :STATUS :ERROR."
  (let ((k::*pixel-tool* hook) (k::*magick* magick) (*png-argv* '())
        (k::*pixel-tool-calls* 0) (k::*pixel-comparisons* 0))
    (handler-case
        (multiple-value-bind (status reasons sha raw)
            (k::capture-pixels (c::cat root "run/") name expected (c::cat root "scratch/"))
          (list :status status :reasons reasons :sha sha :raw raw
                :tools k::*pixel-tool-calls* :comparisons k::*pixel-comparisons*
                :argv (reverse *png-argv*)))
      (error (e) (list :status :error :reasons (list (princ-to-string e)))))))
