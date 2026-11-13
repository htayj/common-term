(defpackage #:lispterm.display
 (:use #:cl) (:nicknames #:ltd)
 (:export #:open-display #:close-display #:reopen-display #:display-open-p #:poll-events
 #:draw-cells #:draw-frame #:set-cursor #:present #:set-title #:display-metrics
 #:make-cell #:cell #:cell-char #:cell-width #:cell-fg #:cell-bg #:cell-bold #:cell-inverse #:cell-underline
 #:palette-color #:char-cell-width #:key-event #:make-key-event #:key-event-key #:key-event-text #:key-event-modifiers #:key-event-bytes
 #:text-event #:make-text-event #:text-event-text #:resize-event #:make-resize-event #:resize-event-columns #:resize-event-rows #:resize-event-width #:resize-event-height
 #:close-event #:make-close-event #:focus-event #:make-focus-event #:focus-event-focused-p #:paste-event #:make-paste-event #:paste-event-text
 #:backend-open #:backend-close #:backend-poll #:backend-blit #:backend-set-title #:backend-metrics
 #:display #:display-backend #:display-backend-state #:display-width #:display-height #:display-columns #:display-rows #:display-font #:display-framebuffer #:display-damage #:display-title #:display-name #:display-configuration
 #:display-renderer-request #:display-renderer-kind #:display-renderer-fallback-reason
 #:synchronize-display-renderer #:renderer-unavailable
 #:cell-p #:key-event-p #:text-event-p #:resize-event-p #:close-event-p #:focus-event-p #:paste-event-p #:display-error-detail
 #:font-width #:font-height #:resize-display #:display-error #:display-open-failed #:display-connection-lost))
(defpackage #:lispterm.display.x11 (:use #:cl #:lispterm.display))
