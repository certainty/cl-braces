(defpackage :cl-braces/compiler/frontend
  (:use #:cl #:serapeum)
  (:export

   :source-location
   :source-origin
   :string-origin
   :file-origin
   :source-uri
   :source-input-open
   :source-input-close
   :call-with-source-input

   :scanner
   :create-scanner
   :next-token
   :eof-p

   :token-keyword
   :+token-kw-func+
   :-token-kw-package+))
