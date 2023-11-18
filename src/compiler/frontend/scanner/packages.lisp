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
   :identifier-p
   :punctuation-p
   :class-any-p

   :@EOF
   :@ILLEGAL
   :@LPAREN
   :@RPAREN
   :@LBRACKET
   :@RBRACKET
   :@LBRACE
   :@RBRACE
   :@INTEGER
   :@PLUS
   :@PLUS_PLUS
   :@MINUS
   :@MINUS_MINUS
   :@STAR
   :@SLASH
   :@LT
   :@LE
   :@GT
   :@GE
   :@SEMICOLON
   :@COLON_EQUAL
   :@IDENTIFIER
   :@TRUE
   :@FALSE
   :@IF
   :@ELSE
   :@BREAK
   :@CONTINUE
   :@FALLTHROUGH
   :@RETURN)
  (:shadow :class))

(defpackage :cl-braces.compiler.frontend.scanner
  (:nicknames :frontend.scanner :scanner)
  (:use :cl)
  (:import-from :serapeum :->)
  (:import-from :alexandria :define-constant :when-let :if-let)
  (:export
   :open-scanner
   :close-scanner
   :call-with-scanner
   :with-input
   :next-token
   :state))
