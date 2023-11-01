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
   :location-line
   :location-column

   :class=
   :token-class

   :source-location

   :@EOF
   :@ILLEGAL
   :@INTEGER)
  (:shadow :class))

(defpackage :cl-braces.compiler.frontend.scanner
  (:nicknames :frontend.scanner :scanner)
  (:use :cl)
  (:import-from :serapeum :-> :defunion :defunit)
  (:export
   :call-with-scanner
   :with-scanner
   :with-input
   :next-token
   :state))
