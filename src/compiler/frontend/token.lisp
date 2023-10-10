(in-package :cl-braces/compiler/frontend)

(defconstant +token-illegal+ :illegal "Returned when scanner failed to recognize input")
(defconstant +token-eof+ :eof "Returned when scanner reached end of input")

(defconstant +token-lparen+ :lparen "The left parenthesis (")
(defconstant +token-rparen+ :rparen "The right parenthesis )")
(defconstant +token-lbrace+ :lbrace "The left brace {")
(defconstant +token-rbrace+ :rbrace "The right brace }")
(defconstant +token-dot+ :dot "The dot .")

(defconstant +token-identifier+ :identifier "Any valid identifier of tinygo")
(defconstant +token-number+ :number "A number literal")
(defconstant +token-string+ :string "A string literal")

(defconstant +token-kw-package+ :package "The package keyword")
(defconstant +token-kw-import+ :import "The import keyword")
(defconstant +token-kw-func+ :func "The func keyword")

(defstruct token
  (type +token-eof+ :type keyword :read-only t)
  (text "" :type string :read-only t)
  (location nil :type (or null source-location) :read-only t))

(-> token-illegal-p (token) boolean)
(defun token-illegal-p (token)
  (eql (token-type token) +token-illegal+))

(-> token-eof-p (token) boolean)
(defun literal-p (token)
  (member (token-type token) '(:number :string)))

(-> token-eof-p (token) boolean)
(defun keyword-p (token)
  (member (token-type token) '(:package :import)))

