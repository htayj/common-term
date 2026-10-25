;;;; Lispterm font. Native Lisp display implementation.

(in-package #:lispterm.display)

(defun load-font (path size)
  (read-otb
   (or path (uiop/os:getenv "LISPTERM_FONT")
       (first
        (directory
         (format nil "/nix/store/*-terminus-font-*/share/fonts/terminus/ter-u~dn.otb" size)))
       (error "Set LISPTERM_FONT to a Terminus OpenType bitmap font"))))

(defun glyph-bitmap (font char)
  (or (gethash char (bitmap-font-cache font))
      (setf (gethash char (bitmap-font-cache font)) (otb-glyph font char))))
