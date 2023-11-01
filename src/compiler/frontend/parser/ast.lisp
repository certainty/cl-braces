(in-package :cl-braces.compiler.frontend.ast)

(defclass node ()
  ((location :reader location
             :initarg :location
             :initform (error "must provide location")
             :type token:location
             :documentation "The location of the start of the node in the source code."))
  (:documentation "The base class for all AST nodes in the highlevel AST."))

(defclass expression (node) ()
  (:documentation "The base class for all expressions in the highlevel AST."))

(defclass bad-expression (expression)
  ((message :reader bad-expression-message
            :initarg :message
            :initform (error "must provide message")
            :type string))
  (:documentation "An expression that could not be parsed correctly."))

(defclass literal (expression)
  ((token :reader literal-token
          :initarg :token
          :initform (error "must provide token")
          :type token:token))
  (:documentation "The base class for all literals in the highlevel AST."))

(-> literal-value (literal) t)
(defun literal-value (expression)
  (token:value (literal-token expression)))
