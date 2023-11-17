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
   :literal-p

   :@EOF
   :@ILLEGAL
   :@LPAREN
   :@RPAREN
   :@LBRACE
   :@RBRACE
   :@INTEGER
   :@PLUS
   :@MINUS
   :@STAR
   :@SLASH
   :@SEMICOLON
   :@COLON_EQUAL
   :@IDENTIFIER
   :@TRUE
   :@FALSE
   :@IF
   :@ELSE)
  (:shadow :class))

(defpackage :cl-braces.compiler.frontend.scanner
  (:nicknames :frontend.scanner :scanner)
  (:use :cl)
  (:import-from :serapeum :->)
  (:import-from :alexandria :define-constant :when-let)
  (:export
   :open-scanner
   :close-scanner
   :call-with-scanner
   :with-input
   :next-token
   :state))
