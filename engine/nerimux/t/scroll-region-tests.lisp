(in-package #:nerimux/test/terminal)

(defmacro define-scroll-region-cases (&body cases)
  "Define scroll-region parser-path cases from declarative rows."
  (labels ((case-option (options key)
             (getf options key))
           (expand-step (step)
             (destructuring-bind (kind &rest args) step
               (ecase kind
                 (:feed `(feed s ,@args))
                 (:feed-lines `(feed-lines s ,@args)))))
           (expand-row-prefix (row expected end message)
             (declare (ignore message))
             `(expect (string= ,expected (row-string s ,row :end ,end))))
           (expand-assertion (assertion)
             (destructuring-bind (kind &rest args) assertion
               (ecase kind
                 (:check-row `(check-row s ,@args))
                 (:row-prefix (apply #'expand-row-prefix args))
                 (:row-blank `(expect (row-blank-p s ,@args))))))
           (expand-case (case)
             (destructuring-bind (name &rest options) case
               (destructuring-bind (width height) (case-option options :screen)
                 `(it ,(string-downcase (symbol-name name))
                      (with-screen (s ,width ,height)
                                   ,@(mapcar #'expand-step
                                             (case-option options :steps))
                                   ,@(mapcar #'expand-assertion
                                             (case-option options :assertions))))))))
    `(progn
       ,@(mapcar #'expand-case cases))))

(describe "terminal-suite/scroll-region"

  (define-scroll-region-cases
    (scroll-auto
     :screen (5 3)
     :steps ((:feed-lines "L1" "L2" "L3" "L4"))
     :assertions ((:check-row 0 "L2")
                  (:check-row 2 "L4")))
    (decstbm-restricts-scroll-to-region
     :screen (5 5)
     :steps ((:feed-lines "R0" "R1" "R2" "R3" "R4")
             (:feed (esc "[2;4r"))
             (:feed (esc "[4;1H"))
             (:feed-lines "" "NR"))
     :assertions ((:row-prefix 0 "R0" 2 "row 0 should be untouched, got ~S")))
    (reverse-index-scrolls-region-down
     :screen (5 3)
     :steps ((:feed-lines "AA" "BB" "CC")
             (:feed (esc "[1;1H"))
             (:feed (esc "M")))
     :assertions ((:row-prefix 1 "AA" 2
                    "after RI, old row 0 should be at row 1; got ~S")
                  (:row-blank 0)))
    (il-insert-lines-pushes-content-down
     :screen (5 4)
     :steps ((:feed-lines "AA" "BB" "CC" "DD")
             (:feed (esc "[2;1H"))
             (:feed (esc "[2L")))
     :assertions ((:check-row 0 "AA")
                  (:row-blank 1)
                  (:row-blank 2)
                  (:check-row 3 "BB")))
    (dl-delete-lines-pulls-content-up
     :screen (5 4)
     :steps ((:feed-lines "AA" "BB" "CC" "DD")
             (:feed (esc "[2;1H"))
             (:feed (esc "[2M")))
     :assertions ((:check-row 0 "AA")
                  (:check-row 1 "DD")
                  (:row-blank 2)
                  (:row-blank 3)))))


(describe "terminal-suite/decstbm-origin-home"

  (it "homes-to-new-top-margin-in-origin-mode"
    (with-screen (s 10 8)
      (feed s (esc "[2;7r"))
      (feed s (esc "[?6h"))
      (feed s (esc "[3;5H"))
      (check-cursor s 4 3)
      (feed s (esc "[4;8r"))
      (expect (nerimux/terminal/types:screen-origin-mode s))
      (expect (= 3 (nerimux/terminal/types:screen-scroll-top s)))
      (expect (= 7 (nerimux/terminal/types:screen-scroll-bottom s)))
      (check-cursor s 0 3)))

  (it "homes-to-screen-top-in-absolute-mode"
    (with-screen (s 10 8)
      (feed s (esc "[2;7r"))
      (feed s (esc "[?6h"))
      (feed s (esc "[?6l"))
      (feed s (esc "[3;5H"))
      (check-cursor s 4 2)
      (feed s (esc "[4;8r"))
      (expect (not (nerimux/terminal/types:screen-origin-mode s)))
      (expect (= 3 (nerimux/terminal/types:screen-scroll-top s)))
      (expect (= 7 (nerimux/terminal/types:screen-scroll-bottom s)))
      (check-cursor s 0 0)))

  (it "direct-action-homes-to-new-top-margin-in-origin-mode"
    (with-screen (s 10 8)
      (nerimux/terminal/actions:decstbm s 1 6)
      (setf (nerimux/terminal/types:screen-origin-mode s) t)
      (nerimux/terminal/actions:set-cursor s 4 5)
      (nerimux/terminal/actions:decstbm s 3 7)
      (expect (= 3 (nerimux/terminal/types:screen-scroll-top s)))
      (expect (= 7 (nerimux/terminal/types:screen-scroll-bottom s)))
      (check-cursor s 0 3)))

  (it "places-vttest-relative-origin-sentences-on-bottom-two-rows"
    ;; vttest main.c do_scrolling: DECOM is active before clear/DECSTBM/LF.
    (with-screen (s 80 24)
      (let ((bottom "Origin mode test. This line should be at the bottom of the screen.")
            (above "This line should be the one above the bottom of the screen. Push <RETURN>"))
        (feed s (esc "[?6h"))
        (feed s (esc "[2J"))
        (feed s (esc "[23;24r"))
        (expect (nerimux/terminal/types:screen-origin-mode s))
        (expect (= 22 (nerimux/terminal/types:screen-scroll-top s)))
        (expect (= 23 (nerimux/terminal/types:screen-scroll-bottom s)))
        (check-cursor s 0 22)
        (feed s (string #\Linefeed))
        (check-cursor s 0 23)
        (feed s bottom)
        (check-cursor s (length bottom) 23)
        (feed s (esc "[1;1H"))
        (check-cursor s 0 22)
        (feed s above)
        (check-cursor s (length above) 22)
        ;; Include all 80 cells, including the untouched trailing blanks.
        (check-row s 23 (format nil "~80A" bottom))
        (check-row s 22 (format nil "~80A" above))))))
