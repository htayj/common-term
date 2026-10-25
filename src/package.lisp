(defpackage #:lispterm
  (:use #:cl)
  (:shadow #:open #:close)
  (:export #:open #:close #:main #:lispterm-error
           #:host #:status #:reopen #:quiesce #:resume #:detach-for-saver
           #:host-error #:host-error-host #:host-timeout #:host-errors #:invalidate #:send-input
           #:input-backpressure #:input-backpressure-pending #:input-backpressure-requested
           #:input-cancelled #:input-cancelled-bytes))
(defpackage #:lispterm.engine.compat
  (:use #:cl)
  (:export #:char-width #:decode-error #:lock #:make-lock #:with-lock-held
           #:octets-to-string #:string-to-octets #:parse-integer-or-nil
           #:split-string #:version-string))
(defpackage #:lispterm.engine
  (:use #:cl)
  (:shadow #:make-screen #:resize #:rows #:columns)
  (:export #:make-screen #:feed #:resize #:rows #:columns #:cursor-row
           #:cursor-column #:cell-at #:cell-character #:cell-width
           #:cell-foreground #:cell-background #:cell-bold-p #:cell-inverse-p
           #:cell-underline-p #:cell-blink-p #:line-size #:logical-columns #:reverse-screen-p
           #:dirty-p #:clear-damage #:palette-color))
