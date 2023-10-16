(in-package :cl-braces/compiler/frontend)

(export '(+token-illegal+ +token-eof))
(defconst +token-illegal+ :illegal "Returned when scanner failed to recognize input")
(defconst +token-eof+ :eof "Returned when scanner reached end of input")

(export '(+token-lparen+ +token-rparen+ +token-lbrace+ +token-rbrace+ +token-lbracket+ +token-rbracket+))
(defconst +token-lparen+ :lparen "The left parenthesis (")
(defconst +token-rparen+ :rparen "The right parenthesis )")
(defconst +token-lbrace+ :lbrace "The left brace {")
(defconst +token-rbrace+ :rbrace "The right brace }")
(defconst +token-lbracket+ :lbracket "The left bracket [")
(defconst +token-rbracket+ :rbracket "The right bracket ]")

(export '(+token-dot+))
(defconst +token-dot+ :dot "The dot .")

(export '(+token-identifier+ +token-number+ +token-string+))
(defconst +token-identifier+ :identifier "Any valid identifier of tinygo")
(defconst +token-number+ :number "A number literal")
(defconst +token-string+ :string "A string literal")

(def *token-class-literal*
  #.(dict
     +token-string+ t
     +token-number+ t))

(export '(+token-op-minus+ +token-op-plus+))

(defconst +token-op-minus+ :op-minus "Minus operator")
(defconst +token-op-plus+ :op-plus "Plus operator")

(def *token-class-operator*
  #.(dict
     +token-op-plus+
     +token-op-minus+))

(export '(+token-kw-package+ +token-kw-import+ +token-kw-func+ +token-kw-var+ +token-kw-return+))
(defconst +token-kw-package+ :kw-package "The package keyword")
(defconst +token-kw-import+ :kw-import "The import keyword")
(defconst +token-kw-func+ :kw-func "The func keyword")
(defconst +token-kw-var+ :kw-var "The var keyword")
(defconst +token-kw-return+ :kw-return "The return keyword")

(def *token-class-keyword*
  #.(dict
     +token-kw-package+ t
     +token-kw-import+ t
     +token-kw-func+ t
     +token-kw-var+ t
     +token-kw-return+ t))

(def *string-to-keyword-type*
  #.(dict
     "func" +token-kw-func+
     "import" +token-kw-import+
     "package" +token-kw-package+
     "var" +token-kw-var+
     "return" +token-kw-return+))

(export '(token token-type token-text token-value token-location))
(defstruct (token (:conc-name token-))
  (type +token-eof+ :type keyword :read-only t)
  (text "" :type string :read-only t)
  (value nil :read-only t)
  (location nil :type (or null source-location) :read-only t))

(export '(token-illegal-p))
(-> token-illegal-p (token) boolean)
(defun token-illegal-p (token)
  (eql (token-type token) +token-illegal+))

(export '(token-eof-p))
(-> token-eof-p (token) boolean)
(defun token-eof-p (token)
  (eql (token-type token) +token-eof+))

(export '(token-literal-p))
(-> token-literal-p (token) boolean)
(defun token-literal-p (token)
  (not (null (gethash (token-type token) *token-class-literal*))))

(export '(token-keyword-p))
(-> token-keyword-p (token) boolean)
(defun token-keyword-p (token)
  (not (null (gethash (token-type token) *token-class-keyword*))))
