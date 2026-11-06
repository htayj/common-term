(in-package #:nerimux/terminal/actions)

;; VT100 User Guide 3.3.5 (ESC F): VT52-compatible mode uses the VT100
;; special graphics repertoire in Table 3-9, not the original VT52 ROM.
(defun %vt52-graphics-char (ch)
  (%dec-graphics-char ch))

(defun %remap-charset-char (screen ch)
  "Map CH through VT52 graphics or the ANSI invoked charset/single shift."
  (if (screen-vt52-mode screen)
      (if (screen-vt52-graphics screen) (%vt52-graphics-char ch) ch)
      (let* ((shift (screen-single-shift screen))
             (charset (if shift (screen-invoked-charset screen shift)
                          (screen-charset screen))))
        (when shift (setf (screen-single-shift screen) nil))
        (case charset
          (:dec-graphics (%dec-graphics-char ch))
          (:british (if (char= ch #\#) #\£ ch))
          (t ch)))))

(defun %consume-pending-wrap (screen)
  "Perform the deferred VT100 wrap before the next printable character."
  (when (and (screen-pending-wrap screen) (not (screen-vt52-mode screen)))
    (%mark-line-wrapped screen (screen-cursor-y screen))
    (setf (screen-pending-wrap screen) nil
          (screen-cursor-x screen) 0)
    (cursor-down/scroll screen)))

(defun %apply-insert-mode-gap (screen ch)
  "Open a display-width gap for CH when insert mode is active."
  (when (screen-insert-mode screen)
    (insert-chars screen (char-width ch))))

(defun write-char-at-cursor (screen ch)
  "Write CH at the cursor according to width, charset, wrap, and insert modes."
  (if (combining-char-p ch)
      (%append-combining-char screen ch)
      (let ((remapped-ch
             (progn
               (%consume-pending-wrap screen)
               (%remap-charset-char screen ch))))
        (setf (screen-last-char screen) remapped-ch)
        (%apply-insert-mode-gap screen remapped-ch)
        (if (= (char-width remapped-ch) 2)
            (%write-wide-cell screen remapped-ch)
            (%write-normal-cell screen remapped-ch)))))

(defun write-codepoint (screen codepoint)
  "Write CODEPOINT at the cursor after converting it with SAFE-CODE-CHAR."
  (write-char-at-cursor screen (safe-code-char codepoint)))
