(in-package :cl-user)

(defpackage :cl-braces.compiler.frontend.token
  (:nicknames :frontend.token :token)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:token
   #:class
   #:lexeme
   #:value
   #:location
   #:token-class
   #:class=
   #:literal-p
   #:identifier-p
   #:punctuation-p
   #:class-any-p

   #:@EOF
   #:@ILLEGAL
   #:@LPAREN
   #:@RPAREN
   #:@LBRACKET
   #:@RBRACKET
   #:@LBRACE
   #:@RBRACE
   #:@INTEGER
   #:@PLUS
   #:@PLUS_PLUS
   #:@MINUS
   #:@MINUS_MINUS
   #:@STAR
   #:@SLASH
   #:@LT
   #:@LE
   #:@GT
   #:@GE
   #:@SEMICOLON
   #:@COMMA
   #:@COLON_EQUAL
   #:@EQUAL
   #:@EQUAL_EQUAL
   #:@IDENTIFIER
   #:@TRUE
   #:@FALSE
   #:@NIL
   #:@IF
   #:@ELSE
   #:@BREAK
   #:@CONTINUE
   #:@FALLTHROUGH
   #:@RETURN
   #:@VAR)
  (:shadow :class))

(defpackage :cl-braces.compiler.frontend.scanner
  (:nicknames :frontend.scanner :scanner)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:input-designator
   #:open-scanner
   #:close-scanner
   #:call-with-scanner
   #:with
   #:with-input
   #:next-token
   #:fail-fast!
   #:state))
