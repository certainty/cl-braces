(in-package :cl-braces.compiler.frontend.ast)

(defgeneric node->sexp (node)
  (:documentation "converts the node to it's sexpression represenation"))

(defclass node ()
  ((id :initform (error "must provide node-id" ) :initarg :id :type positive-fixnum :reader :node-id)
   (location :initform (error "must provide location") :initarg :location :type scanner:source-location :reader :node-location)))

(defclass expression (node) ())

(defclass bad-expression (expression) ())

(defclass literal-expression (expression)
  ((token :initform (error "must provide token") :initarg :token :type scanner:token :reader :literal-expression-token)))

(defclass identifier (expression)
  ((name :initform (error "must provide token") :initarg :name :type string :reader :identifier-name)))

(defclass binary-expression (expression)
  ((operator :reader :binary-expression-operator :initform (error "must provide op") :initarg :operator :type scanner:token)
   (left :reader :binary-expression-left :initform (error "must provide left") :initarg :left :type expression)
   (right :reader :binary-expression-right :initform (error "must provide right") :initarg :right :type expression)))

(defclass unary-expression (expression)
  ((op :reader :unary-expression-op :initform (error "must provide op") :initarg :op :type scanner:token)
   (operand :reader :unary-expression-operand :initform (error "must provide operand") :initarg :operand :type expression)))

(defclass statement (node) ())

(defclass bad-statement (statement) ())

(defclass expression-statement (statement)
  ((expression :initform (error "must provide expr") :initarg :expression :type expression :reader :expression-statement-expression)))


(defclass declaration (node) ())

(defclass const-declaration (declaration)
  ((identifier :initform (error "must provide name") :initarg :identifier :type identifier :reader :const-declaration-identifier)
   (initializer :initform (error "must provide initializer") :initarg :initializer :type expression :reader :const-declaration-initializer)))

(defclass bad-declaration (declaration) ())

(defclass source  (declaration)
  ((declarations :initform (error "must provide declarations") :initarg :declarations :type list :reader :source-declarations)))

(defparameter *include-node-ids* nil)
(defparameter *include-node-locations* nil)
(defparameter *tokens-as-strings* t)
(defparameter *identifiers-as-strings* t)
(defparameter *pretty-print* nil)

(defmethod node->sexp ((expr literal-expression))
  (with-slots (token) expr
    `(literal-expression :tok ,(token->string token))))

(defmethod node->sexp ((expr identifier))
  (with-slots (identifier-name) expr
    `(identifier :name ,(format nil "~A" (if *identifiers-as-strings* identifier-name expr)))))

(defmethod node->sexp ((stm expression-statement))
  (with-slots (expression) stm
    `(expression-statement :expr ,(node->sexp expression))))

(defmethod node->sexp ((decl const-declaration))
  (with-slots (identifier initializer) decl
    `(const-declaration :id ,(node->sexp identifier) :init ,(node->sexp initializer))))


(defun token->string (token)
  (format nil "~A" (if *tokens-as-strings* (scanner:token-text token)  token)))
