(in-package :cl-braces.compiler.frontend.ast)

(defclass node ()
  ((location
    :reader location
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

(defclass statement (node) ())

(defclass expression-statement (statement)
  ((expression
    :reader expression-statement-expression
    :initarg :expression
    :initform (error "must provide expression")
    :type expression))
  (:documentation "A statement that is an expression."))

(defclass declaration (statement) ())

(defclass bad-declaration (declaration)
  ((message :reader bad-declaration-message
            :initarg :message
            :initform (error "must provide message")
            :type string))
  (:documentation "A declaration that could not be parsed correctly."))

(defclass short-variable-declaration (declaration)
  ((identifier
    :reader short-variable-declaration-identifier
    :initarg :identifier
    :initform (error "must provide identifier")
    :type token:token)
   (initializer
    :reader short-variable-declaration-initializer
    :initarg :initializer
    :initform (error "must provide initializer")
    :type expression))
  (:documentation "A declaration of a variable with an initializer"))

(defclass program (node)
  ((declarations
    :reader program-declarations
    :initarg :declarations
    :initform (error "must provide declarations")
    :type list))
  (:documentation "The root node of the highlevel AST."))

(defun make-program (decls)
  (make-instance 'program :declarations decls :location (location:make-source-location 0 0 0)))

;;; Compute spans
(defmethod location:span-for ((node unary-expression))
  (with-slots (operator operand) node
    (let ((operand (span operand)))
      (make-instance 'span
                     :from (token:location operator)
                     :to (span-to operand)))))

(defmethod location:span-for ((node binary-expression))
  (with-slots (lhs rhs) node
    (let ((lhs (span lhs))
          (rhs (span rhs)))
      (make-instance 'span
                     :from (span-from lhs)
                     :to (span-to rhs)))))

(defmethod location:span-for ((node grouping-expression))
  (with-slots (expression) node
    (let ((sub-expr-span (span expression)))
      (make-instance 'span
                     :from (span-from sub-expr-span)
                     :to (span-to sub-expr-span)))))

(defmethod location:span-for ((node literal))
  (with-slots (token) node
    (make-instance 'span
                   :from (token:location token)
                   :to (token:location token))))

(defmethod location:span-for ((node bad-expression))
  (with-slots (location) node
    (make-instance 'span
                   :from location
                   :to location)))

(defmethod location:span-for ((node expression-statement))
  (with-slots (expression) node
    (location:span-for expression)))

(defmethod location:span-for ((node short-variable-declaration))
  (with-slots (identifier initializer) node
    (let ((initializer (location:span-for initializer)))
      (make-instance 'span
                     :from (token:location identifier)
                     :to (location:span-to initializer)))))


;;; AST traversal via the visitor pattern
;;; Visitor for AST nodes
(deftype traversal () '(member inorder postorder))

(declaim (type traversal *traversal*))
(defparameter *traversal* 'inorder "Specifies the way the AST will be traversed. It's a dynamic variable so you can even change the traversal strategy while we traverse")

(defmacro with-preorder-traversal (&body body)
  "Executes `BODY' with the traversal strategy set to preorder. This is really only useful when body is a call to `walk'"
  `(let ((*traversal* 'inorder))
     ,@body))

(defmacro with-postorder-traversal (&body body)
  "Executes `BODY' with the traversal strategy set to postorder. This is really only useful when body is a call to `walk'"
  `(let ((*traversal* 'postorder))
     ,@body))

(defgeneric enter (visitor node)
  (:documentation "Dispatches to the appropriate visit method for the node and visitor"))

(defgeneric leave (visitor node)
  (:documentation "Dispatches to the appropriate leave method for the node and visitor"))

(defmethod leave (visitor (node node)) nil)

(defgeneric walk (visitor node)
  (:documentation "Walks the AST rooted at `NODE', calling the appropriate `visit' and `leave' methods on `VISITOR'. The order in which the nodes are visited is determined by the value of `*traversal*'"))

(defmethod walk (visitor (node node))
  (enter visitor node)
  (leave visitor node))

(defun stop-walking-p (visitor-result)
  (and visitor-result (eq visitor-result :stop)))

(defun continue-walking-p (visitor-result)
  (not (stop-walking-p visitor-result)))

(defmethod walk (visitor (node program))
  (case *traversal*
    (inorder
     (when (continue-walking-p (enter visitor node))
       (dolist (decl (program-declarations node))
         (walk visitor decl))
       (leave visitor node)))
    (postorder
     (dolist (decl (program-declarations node))
       (walk visitor decl))
     (enter visitor node)
     (leave visitor node))))

(defmethod walk (visitor (node expression-statement))
  (case *traversal*
    (inorder
     (when (continue-walking-p (enter visitor node))
       (walk visitor (expression-statement-expression node))
       (leave visitor node)))
    (postorder
     (walk visitor (expression-statement-expression node))
     (enter visitor node)
     (leave visitor node))))

(defmethod walk (visitor (node binary-expression))
  (case *traversal*
    (inorder
     (when (continue-walking-p (enter visitor node))
       (walk visitor (binary-expression-lhs node))
       (walk visitor (binary-expression-rhs node))
       (leave visitor node)))
    (postorder
     (walk visitor (binary-expression-lhs node))
     (walk visitor (binary-expression-rhs node))
     (enter visitor node)
     (leave visitor node))))

(defmethod walk (visitor (node unary-expression))
  (case *traversal*
    (inorder
     (when (continue-walking-p (enter visitor node))
       (walk visitor (unary-expression-operand node))
       (leave visitor node)))
    (postorder
     (walk visitor (unary-expression-operand node))
     (enter visitor node)
     (leave visitor node))))

(defmethod walk (visitor (node grouping-expression))
  (case *traversal*
    (inorder
     (when (continue-walking-p (enter visitor node))
       (walk visitor (grouping-expression-expression node))
       (leave visitor node)))
    (postorder
     (walk visitor (grouping-expression-expression node))
     (enter visitor node)
     (leave visitor node))))
