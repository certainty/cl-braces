(in-package :cl-user)

(defpackage :cl-braces.compiler.frontend.token
  (:nicknames :frontend.token :token)
  (:use :cl)
  (:import-from :serapeum :->)
  (:export
   :token
   :class
   :lexeme
   :value
   :location
   :token-class
   :class=

   :source-location
   :location-offset
   :location-line
   :location-column

   :@EOF
   :@ILLEGAL
   :@INTEGER)
  (:shadow :class))

(defpackage :cl-braces.compiler.frontend.scanner
  (:nicknames :frontend.scanner :scanner)
  (:use :cl)
  (:import-from :serapeum :->)
  (:export
   :open-scanner
   :close-scanner
   :call-with-scanner
   :with-input
   :next-token
   :state))
