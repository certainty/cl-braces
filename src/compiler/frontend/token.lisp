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

(defvar *token-class-literal*
  #.(dict
     +token-string+ t
     +token-number+ t))

(defconstant +token-kw-package+ :kw-package "The package keyword")
(defconstant +token-kw-import+ :kw-import "The import keyword")
(defconstant +token-kw-func+ :kw-func "The func keyword")

(defvar *token-class-keyword*
  #.(dict
     +token-kw-package+ t
     +token-kw-import+ t
     +token-kw-func+ t))

(defvar *string-to-keyword-type*
  #.(dict
     "func" +token-kw-func+
     "import" +token-kw-import+
     "package" +token-kw-package+))

(defstruct token
  (type +token-eof+ :type keyword :read-only t)
  (text "" :type string :read-only t)
  (location nil :type (or null source-location) :read-only t))

(-> token-illegal-p (token) boolean)
(defun token-illegal-p (token)
  (eql (token-type token) +token-illegal+))

(-> token-eof-p (token) boolean)
(defun token-literal-p (token)
  (not (null (gethash (token-type token) *token-class-literal*))))

(-> token-eof-p (token) boolean)
(defun token-keyword-p (token)
  (not (null (gethash (token-type token) *token-class-keyword*))))
