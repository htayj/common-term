;;;; Lispterm cells. Native Lisp display implementation.

(in-package #:lispterm.display)

(defstruct cell (char #\ ) (width 1) fg bg bold inverse underline)

(defstruct key-event key text modifiers)

(defstruct text-event text)

(defstruct resize-event columns rows width height)

(defstruct close-event)

(defstruct focus-event focused-p)

(defstruct paste-event text)

(defun char-cell-width (char)
  (let ((c (char-code char)))
    (cond ((or (< c 32) (<= 768 c 879) (<= 65024 c 65039)) 0)
          ((or (<= 4352 c 4447) (<= 11904 c 42191) (<= 44032 c 55203) (<= 63744 c 64255)
               (<= 127744 c 129791) (<= 131072 c 262143))
           2)
          (t 1))))

(defparameter *ansi-colors*
  #(1908769 13378589 10000154 14129441 4556168 11625094 6856042 11049348 9601908 16468276
    12106534 16432431 8627608 13862555 9355388 15457202))

(defun palette-color (index)
  (check-type index (integer 0 255))
  (cond ((< index 16) (aref *ansi-colors* index))
        ((< index 232)
         (let* ((n (- index 16)) (levels #(0 95 135 175 215 255)))
           (logior (ash (aref levels (floor n 36)) 16)
                   (ash (aref levels (mod (floor n 6) 6)) 8) (aref levels (mod n 6)))))
        (t (* 65793 (+ 8 (* 10 (- index 232)))))))
