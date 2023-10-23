(in-package :cl-braces/compiler/frontend/hr/scanner)

(deftype tpe-token ()
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
    :tok-comma
    :tok-colon
    :tok-semicolon
    :tok-bang
    :tok-eql

    :tok-identifier
    :tok-integer
    :tok-string

    :tok-op-minus
    :tok-op-plus
    :tok-op-double-eql
    :tok-op-bang-eql

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

(deftype tpe-token-literal ()
  '(member
    :tok-integer
    :tok-string))

(deftype tpe-token-operator ()
  '(member
    :tok-op-minus
    :tok-op-plus
    :tok-op-double-eql
    :tok-op-bang-eql))

(deftype tpe-token-keyword ()
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

(defvar *string-to-keyword*
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
  (type :tok-eof :type tpe-token :read-only t)
  (text "" :type string :read-only t)
  (value nil :type t :read-only t)
  (location nil :type (or null source-location) :read-only t))

(-> token-illegal-p (token) boolean)
(defun token-illegal-p (token)
  (eql (token-type token) :tok-illegal))

(-> token-eof-p (token) boolean)
(defun token-eof-p (token)
  (eql (token-type token) :tok-eof))

(-> token-literal-p (token) boolean)
(defun token-literal-p (token)
  (typep (token-type token) 'tpe-token-literal))

(-> token-keyword-p (token) boolean)
(defun token-keyword-p (token)
  (typep (token-type token) 'tpe-token-keyword))
