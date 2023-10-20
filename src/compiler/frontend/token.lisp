(in-package :cl-braces/compiler/frontend)

(deftype token-tpe ()
  '(member
    :tok-illegal
    :tok-eof
    :tok-lparen
    :tok-rparen
    :tok-lbrace
    :tok-rbrace
    :tok-lbracket
    :tok-rbracket
    :tok-dot
    :tok-identifier
    :tok-number
    :tok-string
    :tok-op-minus
    :tok-op-plus
    :tok-kw-package
    :tok-kw-import
    :tok-kw-func
    :tok-kw-var
    :tok-kw-return
    :tok-kw-if
    :tok-kw-else
    :tok-kw-switch
    :tok-kw-case
    :tok-kw-default
    :tok-kw-break
    :tok-kw-for
    :tok-kw-range))

(deftype token-literal-tpe ()
  '(member
    :tok-number
    :tok-string))

(deftype token-operator-tpe ()
  '(member
    :tok-op-minus
    :tok-op-plus))

(deftype token-keyword-tpe ()
  '(member
    :tok-kw-package
    :tok-kw-import
    :tok-kw-func
    :tok-kw-var
    :tok-kw-return
    :tok-kw-if
    :tok-kw-else
    :tok-kw-switch
    :tok-kw-case
    :tok-kw-default
    :tok-kw-break
    :tok-kw-for
    :tok-kw-range))

(def *string-to-keyword-type*
  #.(dict
     "func" :tok-kw-func
     "import" :tok-kw-import
     "package" :tok-kw-package
     "var" :tok-kw-var
     "return" :tok-kw-return
     "if" :tok-kw-if
     "else" :tok-kw-else
     "switch" :tok-kw-switch
     "case" :tok-kw-case
     "default" :tok-kw-default
     "break" :tok-kw-break
     "for" :tok-kw-for
     "range" :tok-kw-range))


(defstruct (token (:conc-name token-))
  (type :tok-eof :type token-tpe :read-only t)
  (text "" :type string :read-only t)
  (value nil :read-only t)
  (location nil :type (or null source-location) :read-only t))

(-> token-illegal-p (token token-tpe) boolean)
(defun token-illegal-p (token)
  (eql (token-type token) :tok-illegal))

(-> token-eof-p (token token-tpe) boolean)
(defun token-eof-p (token)
  (eql (token-type token) :tok-eof))

(-> token-literal-p (token token-tpe) boolean)
(defun token-literal-p (token)
  (typep (token-type token) 'token-literal-tpe))

(-> token-keyword-p (token token-tpe) boolean)
(defun token-keyword-p (token)
  (typep (token-type token) 'token-keyword-tpe))
