(in-package :cl-braces.compiler.frontend.ast)

(defclass node ()
  ((id :initform (error "must provide node-id" ) :initarg :id :type positive-fixnum :reader :node-id)
   (location :initform (error "must provide location") :initarg :location :type scanner:source-location :reader :node-location)))

(defclass expression (node) ())

(defclass bad-expression (expression) ())

(defclass literal-expression (expression)
  ((token :initform (error "must provide token") :initarg :token :type scanner:token :reader :literal-expression-token)))

(defclass identifier (expression)
  ((name :initform (error "must provide token") :initarg :name :type string :reader :identifier-name)))

(defclass statement (node) ())

(defclass bad-statement (statement) ())

(defclass expression-statement (statement)
  ((expression :initform (error "must provide expr") :initarg :expresion :type expression :reader :expression-statement-expression)))

(defclass declaration (node) ())

(defclass const-declaration (declaration)
  ((identifier :initform (error "must provide name") :initarg :identifier :type identifier :reader :const-declaration-identifier)
   (initializer :initform (error "must provide initializer") :initarg :initializer :type expression :reader :const-declaration-initializer)))

(defclass bad-declaration (declaration) ())

(defclass source  (declaration)
  ((declarations :initform (error "must provide declarations") :initarg :declarations :type list :reader :source-declarations)))
