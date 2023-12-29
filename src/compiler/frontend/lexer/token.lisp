(in-package :cl-braces.compiler.frontend.token)
;;;; This file provides data structures and functions to represent lexical tokens in our little language.
;;;; It follows the golang language specifcation for lexical tokens (https://golang.org/ref/spec#Tokens).


;;; The `token-class' type is an enumeration of all the possible token types.
;;; The language specification defines 6 overarching categories of tokens:
;;; 1. Identifiers
;;; 2. Keywords (which are just identifiers that have special meaning in the language))
;;; 3. Literals
;;; 4. Operators
;;; 5. Punctuation
;;; 6. Special (i.e. EOF, ILLEGAL)

(s:defunion token-class

  ;; [Identifiers](https://golang.org/ref/spec#Identifiers)
  @IDENTIFIER

  ;; [Keywords](https://golang.org/ref/spec#Keywords)
  @IF
  @ELSE
  @BREAK
  @CONTINUE
  @FALLTHROUGH
  @RETURN
  @VAR
  @FUNC

  ;; [Literals](https://golang.org/ref/spec#Literals)
  @INTEGER

  ;; this is a deviation from golang, which doesn't declare these as literals, but rather as predeclared constants
  @TRUE
  @FALSE
  @NIL

  ;; [Operators](https://golang.org/ref/spec#Operators_and_punctuation)
  @PLUS
  @PLUS_PLUS
  @MINUS
  @MINUS_MINUS
  @STAR
  @SLASH
  @COLON_EQUAL
  @EQUAL
  @EQUAL_EQUAL
  @MUL_EQUAL
  @PLUS_EQUAL
  @LT
  @LE
  @GT
  @GE
  @ELLIPSIS
  @AMPERSAND
  @AMPERSAND_AMPERSAND
  @AMPERSAND_EQUAL
  @PIPE
  @PIPE_PIPE
  @PIPE_EQUAL
  @CARET
  @CARET_EQUAL
  @TILDE
  @TILDE_EQUAL
  @BANG
  @BANG_EQUAL

  ;; [Punctuation](https://golang.org/ref/spec#Operators_and_punctuation)
  @LPAREN
  @RPAREN
  @LBRACKET
  @RBRACKET
  @LBRACE
  @RBRACE
  @SEMICOLON
  @COMMA
  @DOT

  ;; Special
  @ILLEGAL ; This is used to represent an illegal token. This is used so that we can delay the decision what to do in this case.
  @EOF)

(defclass token ()
  ((class
    :reader class
    :initarg :class
    :initform (error "no type given")
    :type token-class
    :documentation "The type of the token, which is a member of `token-class'")
   (lexeme
    :reader lexeme
    :initarg :lexeme
    :initform (error "no lexeme given")
    :type string
    :documentation "The actual string that was matched by the scanner.")
   (value
    :reader value
    :initarg :value
    :initform nil
    :type (or null t)
    :documentation
    "The value of the token. This is used mostly for literals, which we can evaluate at compile time to lisp values.
    These are not necessarily equivalent to the runtime values we will eventually get.")
   (span
    :reader span
    :initarg :span
    :initform nil
    :type (or null span:source-span)
    :documentation "The span of the token in the source file.")
   (location
    :reader location
    :initarg :location
    :initform nil
    :type (or null location:source-location)
    :documentation "The location in the source file where this token was found. For tokens that match multiple characters, this is the location of the first character in the token."))
  (:documentation "A token is a single unit of input. It is defined by the `class' and it's `lexeme'.
   The `class' represents the type of the token.
   The `lexeme' represents the actual string that was matched by the scanner.
   For many tokens, the `lexeme' does not contain any useful information.
   For example, the `lexeme' for the `PLUS' token is the string \"+\".
   However, for some tokens, the `lexeme' is very important.
   For example, the `lexeme' for an `identifier' token is the actual identifier.
   "))

(defmethod print-object ((token token) stream)
  (with-slots (class lexeme value span) token
    (print-unreadable-object (token stream :type t :identity t)
      (format stream "class: ~a lexeme: ~a value: ~a span: ~a" class lexeme value span))))

(defmethod support:to-plist ((token token))
  (with-slots (class lexeme value span) token
    (list :class class :lexeme lexeme :value value :span (support:to-plist span))))

(defmethod span:for ((token token))
  (with-slots (span) token
    span))

(defun synthetic-eof ()
  "Returns a synthetic EOF token. This is used to mark the end of the input."
  (make-instance 'token
                 :class @EOF
                 :lexeme ""
                 :location (make-instance 'location:source-location
                                          :line 0
                                          :column 0
                                          :offset 0)))

(-> class= (token token-class) boolean)
(defun class= (token expected-class)
  "Returns `t' if the `tokens' class is equal to the given `expected-class.'"
  (with-slots (class) token
    (eql class expected-class)))

(-> keyword-p (token) boolean)
(defun class-any-p (token &rest expected-classes)
  "Returns `t' if the `tokens' class is equal to any of the given `expected-classes.'"
  (with-slots (class) token
    (not (null (member class expected-classes :test #'eql)))))

(-> keyword-p (token) boolean)

(-> literal-p (token) boolean)
(defun literal-p (token)
  "Returns `t' if the `token' is a literal."
  (class-any-p token @INTEGER @TRUE @FALSE @NIL))

(-> identifier-p (token) boolean)
(defun identifier-p (token)
  "Returns `t' if the `token' is an identifier."
  (class= token @IDENTIFIER))

(-> punctuation-p (token) boolean)
(defun punctuation-p (token)
  "Returns `t' if the `token' is a punctuation token."
  (class-any-p token @LPAREN @RPAREN @LBRACE @RBRACE @LBRACKET @RBRACKET))
