(in-package :cl-user)

(defpackage :cl-braces.compiler.frontend.scanner
  (:nicknames :frontend.scanner :scanner)
  (:use :cl)
  (:import-from :serapeum :->)
  (:export
   :call-with-scanner
   :with-scanner
   :next-token

   :token
   :token-type
   :token-value
   :token-text

   :tpe-token-type
   :tok-eof
   :tok-illegal
   :tok-integer
   ))
