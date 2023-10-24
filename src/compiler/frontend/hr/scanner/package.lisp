(defpackage #:cl-braces/compiler/frontend/hr/scanner
  (:use #:cl #:serapeum)
  (:export
   #:source-origin
   #:file-origin
   #:file-origin-path
   #:string-origin
   #:string-origin-label
   #:source-uri
   #:source-input
   #:source-input-stream
   #:source-input-origin
   #:source-input-open
   #:source-input-close
   #:call-with-source-input
   #:with-source-input

   #:source-location
   #:location-line
   #:location-column
   #:location-offset

   #:token
   #:token-literal-p
   #:token-keyword-p
   #:token-eof-p
   #:token-illegal-p
   #:token-type
   #:token-value
   #:token-text
   #:token-location

   #:scan-state
   #:make-scan-state
   #:scan-errors
   #:scan-error
   #:scan-error-message
   #:scan-error-location
   #:scan-origin
   #:eof-p

   #:next-token
   #:string->scanner
   #:scan-all
   ))
