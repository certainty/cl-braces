(in-package :cl-user)

(defpackage :cl-braces.compiler.frontend.token
  (:nicknames :frontend.token :token)
  (:use :cl)
  (:import-from :serapeum :->)
  (:export
   :token
   :token-class
   :token-lexeme
   :token-value

   :token-class
   :class=

   :location

   :@EOF
   :@ILLEGAL
   :@INTEGER))

(defpackage :cl-braces.compiler.frontend.scanner
  (:nicknames :frontend.scanner :scanner)
  (:use :cl)
  (:import-from :serapeum :->)
  (:export
   :call-with-scanner
   :with-scanner
   :next-token))
