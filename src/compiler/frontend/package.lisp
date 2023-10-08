(defpackage :cl-braces/compiler/frontend
  (:use :cl)
  (:export
   :source-origin
   :string-origin
   :file-origin
   :source-uri
   :create-source-input
   :source-location

   :create-scanner
   :next-token
   :test-func
   ))
