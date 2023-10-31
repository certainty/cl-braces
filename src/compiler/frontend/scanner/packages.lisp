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

   :class=
   :token-class-t

   :source-location

   :@EOF
   :@ILLEGAL
   :@INTEGER)
  (:shadow :class))

(defpackage :cl-braces.compiler.frontend.scanner
  (:nicknames :frontend.scanner :scanner)
  (:use :cl)
  (:import-from :serapeum :->)
  (:export
   :call-with-scanner
   :with-scanner
   :next-token))
