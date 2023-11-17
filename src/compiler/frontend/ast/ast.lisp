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

(defclass bad-statement (statement)
  ((message :reader bad-statement-message
            :initarg :message
            :initform (error "must provide message")
            :type string))
  (:documentation "A statement that could not be parsed correctly."))

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

(defclass variable (node)
  ((identifier
    :reader variable-identifier
    :initarg :identifier
    :initform (error "must provide identifier")
    :type token:token))
  (:documentation "The base class for all variables in the highlevel AST."))

(defclass short-variable-declaration (declaration)
  ((variable
    :reader short-variable-declaration-variable
    :initarg :variable
    :initform (error "must provide identifier")
    :type variable)
   (initializer
    :reader short-variable-declaration-initializer
    :initarg :initializer
    :initform (error "must provide initializer")
    :type expression))
  (:documentation "A declaration of a variable with an initializer"))

(defclass block (node)
  ((statements
    :reader block-statements
    :initarg :statements
    :initform (error "must provide statements")
    :type list))
  (:documentation "A block of statements"))

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

(defmethod location:span-for ((node variable))
  (with-slots (identifier) node
    (make-instance 'span
                   :from (token:location identifier)
                   :to (token:location identifier))))

(defmethod location:span-for ((node block))
  (with-slots (statements) node
    (let ((first-statement (first statements))
          (last-statement  (first (last statements))))
      (make-instance 'span
                     :from (span-from first-statement)
                     :to (span-to last-statement)))))

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

(defgeneric children (node)
  (:documentation "Returns a list of the children of `NODE'"))

(defgeneric enter (visitor node)
  (:documentation "Dispatches to the appropriate visit method for the node and visitor"))

(defgeneric leave (visitor node)
  (:documentation "Dispatches to the appropriate leave method for the node and visitor"))

(defmethod leave (visitor (node node)) nil)

(defgeneric walk (visitor node)
  (:documentation "Walks the AST rooted at `NODE', calling the appropriate `visit' and `leave' methods on `VISITOR'. The order in which the nodes are visited is determined by the value of `*traversal*'"))

(defun stop-walking-p (visitor-result)
  (and visitor-result (eq visitor-result :stop)))

(defun continue-walking-p (visitor-result)
  (not (stop-walking-p visitor-result)))

(defmethod walk (visitor (node node))
  (case *traversal*
    (inorder
     (when (continue-walking-p (enter visitor node))
       (dolist (child (children node))
         (walk visitor child))
       (leave visitor node)))
    (postorder
     (dolist (child (children node))
       (walk visitor child))
     (enter visitor node)
     (leave visitor node))))

(defmethod children ((node program))
  (program-declarations node))

(defmethod children ((node bad-declaration))
  nil)

(defmethod children ((node bad-statement))
  nil)

(defmethod children ((node expression-statement))
  (list (expression-statement-expression node)))

(defmethod children ((node binary-expression))
  (list (binary-expression-lhs node)
        (binary-expression-rhs node)))

(defmethod children ((node unary-expression))
  (list (unary-expression-operand node)))

(defmethod children ((node grouping-expression))
  (list (grouping-expression-expression node)))

(defmethod children ((node literal))
  nil)

(defmethod children ((node bad-expression))
  nil)

(defmethod children ((node short-variable-declaration))
  (list
   (short-variable-declaration-variable node)
   (short-variable-declaration-initializer node)))

(defmethod children ((node variable))
  nil)

(defmethod children ((node block))
  (block-statements node))
