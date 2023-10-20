(defpackage #:cl-braces/compiler/frontend
  (:use #:cl #:serapeum)
  (:export

   #:token-literal-p
   #:token-keyword-p
   #:token-eof-p
   #:token-illegal-p
   #:token-type
   #:token-value
   #:token-location

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

   #:scan-state
   #:make-scan-state
   #:scan-errors
   #:scan-error
   #:scan-error-message
   #:scan-error-location

   #:next-token
   #:string->scanner
   #:scan-all
   ))
