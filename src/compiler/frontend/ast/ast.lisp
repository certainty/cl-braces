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
  ((token
    :reader literal-token
    :initarg :token
    :initform (error "must provide token")
    :type token:token))
  (:documentation "The base class for all literals in the highlevel AST."))

(-> literal-value (literal) t)
(defun literal-value (expression)
  (token:value (literal-token expression)))

(defclass grouping-expression (expression)
  ((expression
    :reader grouping-expression-expression
    :initarg :expression
    :initform (error "must provide expression")
    :type expression))
  (:documentation "An expression that is surrounded by parentheses."))

(defclass unary-expression (expression)
  ((operator
    :reader unary-expression-operator
    :initarg :operator
    :initform (error "must provide op")
    :type token:token)
   (operand
    :reader unary-expression-operand
    :initarg :operand
    :initform (error "must provide operand")
    :type expression))
  (:documentation "An expression for binary relations"))

(defclass binary-expression (expression)
  ((lhs
    :reader binary-expression-lhs
    :initarg :lhs
    :initform (error "must provide lhs")
    :type expression)
   (operator
    :reader binary-expression-operator
    :initarg :operator
    :initform (error "must provide operator")
    :type token:token)
   (rhs
    :reader binary-expression-rhs
    :initarg :rhs
    :initform (error "must provide rhs")
    :type expression))
  (:documentation "An expression for binary relations"))

;;; Compute spans over expressions
(defclass span ()
  ((from :reader span-from
         :initarg :from
         :initform (error "must provide from")
         :type token:location
         :documentation "This is the location of the the first token or subexpression of the expression."
         )
   (to :reader span-to
       :initarg :to
       :initform (error "must provide to")
       :type token:location
       :documentation "This is the location of the last token or subexpression of the expression."))
  (:documentation "A span in the source code."))

(defmethod print-object ((span span) stream)
  (with-slots (from to) span
    (print-unreadable-object (span stream :type t :identity t)
      (format stream "[~A, ~A]" from to))))

(defgeneric span (expression)
  (:documentation "Computes the span of the expression in the source code."))

(defmethod span ((node unary-expression))
  (with-slots (operator operand) node
    (let ((operand (span operand)))
      (make-instance 'span
                     :from (token:location operator)
                     :to (span-to operand)))))

(defmethod span ((node binary-expression))
  (with-slots (lhs rhs) node
    (let ((lhs (span lhs))
          (rhs (span rhs)))
      (make-instance 'span
                     :from (span-from lhs)
                     :to (span-to rhs)))))

(defmethod span ((node grouping-expression))
  (with-slots (expression) node
    (let ((sub-expr-span (span expression)))
      (make-instance 'span
                     :from (span-from sub-expr-span)
                     :to (span-to sub-expr-span)))))

(defmethod span ((node literal))
  (with-slots (token) node
    (make-instance 'span
                   :from (token:location token)
                   :to (token:location token))))

;;; Visitor for AST nodes
(defgeneric enter (visitor node)
  (:documentation "Dispatches to the appropriate visit method for the node and visitor"))

(defgeneric leave (visitor node)
  (:documentation "Dispatches to the appropriate leave method for the node and visitor"))

(defgeneric walk (visitor node)
  (:documentation "Walks the AST rooted at `NODE', calling the appropriate `visit' and `leave' methods on `VISITOR'."))

(defmethod walk (visitor (node node))
  (enter visitor node)
  (leave visitor node))

(defmethod walk (visitor (node binary-expression))
  (enter visitor node)
  (walk visitor (binary-expression-lhs node))
  (walk visitor (binary-expression-rhs node))
  (leave visitor node))

(defmethod walk (visitor (node unary-expression))
  (enter visitor node)
  (walk visitor (unary-expression-operand node))
  (leave visitor node))

(defmethod walk (visitor (node grouping-expression))
  (enter visitor node)
  (walk visitor (grouping-expression-expression node))
  (leave visitor node))
