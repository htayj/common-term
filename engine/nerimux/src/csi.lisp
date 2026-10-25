(in-package #:nerimux/terminal/csi)

(define-csi-rule-set csi-screen-rules

  ((and (null intermed) (char= final #\A))
   (cursor-up screen p1*))

  ((and (null intermed) (char= final #\B))
   (cursor-down screen p1*))

  ((and (null intermed) (char= final #\C))
   (set-cursor screen (+ (screen-cursor-x screen) p1*) (screen-cursor-y screen)))

  ((and (null intermed) (char= final #\D))
   (set-cursor screen (- (screen-cursor-x screen) p1*) (screen-cursor-y screen)))

  ((and (null intermed) (char= final #\E))
   (cursor-down screen p1*)
   (cursor-cr screen))

  ((and (null intermed) (char= final #\F))
   (cursor-up screen p1*)
   (cursor-cr screen))

  ((and (null intermed) (char= final #\G))
   (set-cursor screen (1- p1*) (screen-cursor-y screen)))

  ((and (null intermed) (char= final #\`))
   (set-cursor screen (1- p1*) (screen-cursor-y screen)))

  ((and (null intermed) (char= final #\a))
   (set-cursor screen (+ (screen-cursor-x screen) p1*) (screen-cursor-y screen)))

  ((and (null intermed) (char= final #\e))
   (cursor-down screen p1*))

  ((and (null intermed) (char= final #\s))
   (save-cursor screen))

  ((and (null intermed) (char= final #\u))
   (restore-cursor screen))

  ((and (null intermed) (char= final #\H))
   (set-cursor screen (1- p2*) (%cup-row screen p1*)))

  ((and (null intermed) (char= final #\@))
   (insert-chars screen p1*))

  ((and (null intermed) (char= final #\J))
   (erase-display screen p1))

  ((and (null intermed) (char= final #\K))
   (erase-line screen p1))

  ((and (null intermed) (char= final #\L))
   (insert-lines screen p1*))

  ((and (null intermed) (char= final #\M))
   (delete-lines screen p1*))

  ((and (null intermed) (char= final #\P))
   (delete-chars screen p1*))

  ((and (null intermed) (char= final #\S))
   (dotimes (_ p1*) (scroll-up-one screen)))

  ((and (null intermed) (char= final #\T))
   (dotimes (_ p1*) (scroll-down-one screen)))

  ((and (null intermed) (char= final #\X))
   (erase-region screen
                 (screen-cursor-x screen) (screen-cursor-y screen)
                 (min (+ (screen-cursor-x screen) p1* -1)
                      (1- (screen-width screen)))
                 (screen-cursor-y screen)))

  ((and (null intermed) (char= final #\b))
   (let ((preceding-char (screen-last-char screen))
         (count          (if params (first params) 1)))
     (when preceding-char
       (dotimes (_ count) (write-char-at-cursor screen preceding-char)))))

  ((and (null intermed) (char= final #\d))
   (set-cursor screen (screen-cursor-x screen) (1- p1*)))

  ((and (null intermed) (char= final #\f))
   (set-cursor screen (1- p2*) (%cup-row screen p1*)))

  ((and (null intermed) (char= final #\m))
   (apply-sgr screen params)))
