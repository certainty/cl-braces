(in-package :cl-braces/compiler/frontend)

(defconstant +token-illegal+ :illegal "Returned when scanner failed to recognize input")
(defconstant +token-eof+ :eof "Returned when scanner reached end of input")

(defconstant +token-lparen+ :lparen "The left parenthesis (")
(defconstant +token-rparen+ :rparen "The right parenthesis )")
(defconstant +token-lbrace+ :lbrace "The left brace {")
(defconstant +token-rbrace+ :rbrace "The right brace }")
(defconstant +token-lbracket+ :lbracket "The left bracket [")
(defconstant +token-rbracket+ :rbracket "The right bracket ]")

(defconstant +token-dot+ :dot "The dot .")

(defconstant +token-identifier+ :identifier "Any valid identifier of tinygo")
(defconstant +token-number+ :number "A number literal")
(defconstant +token-string+ :string "A string literal")

(defvar *token-class-literal*
  #.(dict
     +token-string+ t
     +token-number+ t))

(defconstant +token-op-minus+ :op-minus "Minus operator")
(defconstant +token-op-plus+ :op-plus "Plus operator")

(defvar *token-class-operator*
  #.(dict
     +token-op-plus+
     +token-op-minus+))

(defconstant +token-kw-package+ :kw-package "The package keyword")
(defconstant +token-kw-import+ :kw-import "The import keyword")
(defconstant +token-kw-func+ :kw-func "The func keyword")
(defconstant +token-kw-var+ :kw-var "The var keyword")
(defconstant +token-kw-return+ :kw-return "The return keyword")

(defvar *token-class-keyword*
  #.(dict
     +token-kw-package+ t
     +token-kw-import+ t
     +token-kw-func+ t
     +token-kw-var+ t
     +token-kw-return+ t))

(defvar *string-to-keyword-type*
  #.(dict
     "func" +token-kw-func+
     "import" +token-kw-import+
     "package" +token-kw-package+
     "var" +token-kw-var+
     "return" +token-kw-return+))

(defstruct token
  (type +token-eof+ :type keyword :read-only t)
  (text "" :type string :read-only t)
  (value nil :read-only t)
  (location nil :type (or null source-location) :read-only t))

(-> token-illegal-p (token) boolean)
(defun token-illegal-p (token)
  (eql (token-type token) +token-illegal+))

(-> token-eof-p (token) boolean)
(defun token-eof-p (token)
  (eql (token-type token) +token-eof+))

(-> token-literal-p (token) boolean)
(defun token-literal-p (token)
  (not (null (gethash (token-type token) *token-class-literal*))))

(-> token-keyword-p (token) boolean)
(defun token-keyword-p (token)
  (not (null (gethash (token-type token) *token-class-keyword*))))
