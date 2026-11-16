(defpackage #:lispterm.test.engine (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect))
(in-package #:lispterm.test.engine)
(describe "lispterm engine adapter"
  (it "exposes dimensions, cursor, and damage"
    (let ((screen (lispterm.engine:make-screen :columns 10 :rows 3)))
      (lispterm.engine:clear-damage screen)
      (assert (not (lispterm.engine:dirty-p screen)))
      (lispterm.engine:feed screen (babel:string-to-octets "hi"))
      (assert (lispterm.engine:dirty-p screen))
      (assert (= 2 (lispterm.engine:cursor-column screen)))
      (assert (= 0 (lispterm.engine:cursor-row screen)))
      (assert (char= #\h (lispterm.engine:cell-character (lispterm.engine:cell-at screen 0 0))))
      (lispterm.engine:resize screen 4 12)
      (assert (= 4 (lispterm.engine:rows screen)))
      (assert (= 12 (lispterm.engine:columns screen)))))
  (it "resolves indexed/default/true colours and attributes"
    (let ((screen (lispterm.engine:make-screen)))
      (lispterm.engine:feed screen (babel:string-to-octets (format nil "~C[1;4;7;38;2;12;34;56mX" #\Esc)))
      (let ((cell (lispterm.engine:cell-at screen 0 0)))
        (assert (= #x0c2238 (lispterm.engine:cell-foreground cell)))
        (assert (null (lispterm.engine:cell-background cell)))
        (assert (lispterm.engine:cell-bold-p cell))
        (assert (lispterm.engine:cell-inverse-p cell))
        (assert (lispterm.engine:cell-underline-p cell))))
    (assert (= #xffffff (lispterm.engine:palette-color 231)))
    (assert (= #x080808 (lispterm.engine:palette-color 232))))
  (it "uses the CGA palette for regular, bright and indexed SGR colors"
    (loop for expected across #(#x000000 #xaa0000 #x00aa00 #xaa5500
                                #x0000aa #xaa00aa #x00aaaa #xaaaaaa
                                #x555555 #xff5555 #x55ff55 #xffff55
                                #x5555ff #xff55ff #x55ffff #xffffff)
          for index from 0 do
            (assert (= expected (lispterm.engine:palette-color index))))
    (dolist (case '(("31;44" #xaa0000 #x0000aa)
                    ("91;104" #xff5555 #x5555ff)
                    ("38;5;3;48;5;6" #xaa5500 #x00aaaa)
                    ("38;5;67;48;5;232" #x5f87af #x080808)
                    ("31;44;39;49" nil nil)
                    ("31;44;0" nil nil)))
      (destructuring-bind (sgr foreground background) case
        (let ((screen (lispterm.engine:make-screen)))
          (lispterm.engine:feed screen
            (babel:string-to-octets (format nil "~C[~AmX" #\Esc sgr)))
          (let ((cell (lispterm.engine:cell-at screen 0 0)))
            (assert (eql foreground (lispterm.engine:cell-foreground cell)))
            (assert (eql background (lispterm.engine:cell-background cell))))))))
  (it "uses Unicode widths for CJK, combining marks, and watch"
    (assert (= 2 (lispterm.engine.compat:char-width #\中)))
    (assert (= 0 (lispterm.engine.compat:char-width (code-char #x301))))
    (assert (= 2 (lispterm.engine.compat:char-width (code-char #x231a)))))
  (it "tracks wide leading and continuation cells"
    (let ((screen (lispterm.engine:make-screen)))
      (lispterm.engine:feed screen (babel:string-to-octets "中"))
      (assert (= 2 (lispterm.engine:cell-width (lispterm.engine:cell-at screen 0 0))))
      (assert (= 0 (lispterm.engine:cell-width (lispterm.engine:cell-at screen 0 1)))))))
