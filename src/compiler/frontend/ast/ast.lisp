(in-package :cl-braces.compiler.frontend.ast)

(defclass node ()
  ((location
    :reader location
    :initarg :location
    :initform (error "must provide location")
    :type token:location
    :documentation "The location of the start of the node in the source code."))
  (:documentation "The base class for all AST nodes in the highlevel AST."))

;;; ===========================================================================
;;; AST traversal via the visitor pattern
;;; ===========================================================================

(defgeneric walk (visitor node)
  (:documentation "Walks the AST rooted at `NODE', calling the appropriate `visit' and `leave' methods on `VISITOR'. The tree is traversed inorder."))

(defgeneric children (node)
  (:documentation "Returns a list of the children of `NODE'"))

(defgeneric enter (visitor node)
  (:documentation "Dispatches to the appropriate visit method for the node and visitor"))

(defgeneric leave (visitor node)
  (:documentation "Dispatches to the appropriate leave method for the node and visitor"))

(defun stop-walking-p (visitor-result)
  (and visitor-result (eq visitor-result :stop)))

(defun continue-walking-p (visitor-result)
  (not (stop-walking-p visitor-result)))

(defmethod walk (visitor (node node))
  (when (continue-walking-p (enter visitor node))
    (dolist (child (children node))
      (walk visitor child))
    (leave visitor node)))

;;; ===========================================================================
;;; Program
;;; ===========================================================================

(defclass program (node)
  ((declarations
    :reader program-declarations
    :initarg :declarations
    :initform (error "must provide declarations")
    :type statement-list))
  (:documentation "The root node of the highlevel AST."))

(defun make-program (decls)
  (make-instance 'program :declarations decls :location (location:make-source-location 0 0 0)))

(defmethod children ((node program))
  (list (program-declarations node)))

(defmethod location:span-for ((node program))
  (with-slots (declarations) node
    (let ((first-declaration (first declarations))
          (last-declaration  (first (last declarations))))
      (make-instance 'span
                     :from (span-from first-declaration)
                     :to (span-to last-declaration)))))

;;; ===========================================================================
;;; Statements
;;; ===========================================================================

(defclass statement (node) ())

(defclass bad-statement (statement)
  ((message :reader bad-statement-message
            :initarg :message
            :initform (error "must provide message")
            :type string))
  (:documentation "A statement that could not be parsed correctly."))

(defmethod children ((node bad-statement))
  nil)

(defmethod location:span-for ((node bad-statement))
  (with-slots (location) node
    (make-instance 'span
                   :from location
                   :to location)))

(defclass empty-statement (statement) ())

(defmethod children ((node empty-statement))
  nil)

(defmethod location:span-for ((node empty-statement))
  (with-slots (location) node
    (make-instance 'span
                   :from location
                   :to location)))

(defclass statement-list (statement)
  ((statements
    :reader statement-list-statements
    :initarg :statements
    :initform (error "must provide statements")
    :type list))
  (:documentation "A statement that is a list of statements"))

(defmethod children ((node statement-list))
  (statement-list-statements node))

(defmethod location:span-for ((node statement-list))
  (with-slots (statements) node
    (let ((first-statement (first statements))
          (last-statement  (first (last statements))))
      (make-instance 'span
                     :from (span-from first-statement)
                     :to (span-to last-statement)))))

(defclass expression-statement (statement)
  ((expression
    :reader expression-statement-expression
    :initarg :expression
    :initform (error "must provide expression")
    :type expression))
  (:documentation "A statement that is an expression."))

(defmethod children ((node expression-statement))
  (list (expression-statement-expression node)))

(defmethod location:span-for ((node expression-statement))
  (with-slots (expression) node
    (location:span-for expression)))

(defclass if-statement (statement)
  ((init
    :reader if-statement-init
    :initarg :init
    :initform nil
    :type (or empty-statement simple-statement))

   (condition
    :reader if-statement-condition
    :initarg :condition
    :initform (error "must provide condition")
    :type expression)

   (consequence
    :reader if-statement-consequence
    :initarg :consequence
    :initform (error "must provide then-block")
    :type block)

   (alternative
    :reader if-statement-alternative
    :initarg :alternative
    :initform nil
    :type (or null statement)))
  (:documentation "A statement that is an expression."))

(defmethod children ((node if-statement))
  (let ((base (list
               (if-statement-init node)
               (if-statement-condition node)
               (if-statement-consequence node))))
    (when (if-statement-alternative node)
      (setf base (append base (list (if-statement-alternative node)))))
    base))

(defmethod location:span-for ((node if-statement))
  (with-slots (condition consequence alternative) node
    (let ((condition (location:span-for condition))
          (consequence (location:span-for consequence))
          (alternative (location:span-for alternative)))
      (make-instance 'span
                     :from (span-from condition)
                     :to (span-to (or alternative consequence))))))

(defclass block (node)
  ((statements
    :reader block-statements
    :initarg :statements
    :initform (error "must provide statements")
    :type statement-list))
  (:documentation "A block of statements"))

(defmethod children ((node block))
  (list (block-statements node)))

(defmethod location:span-for ((node block))
  (with-slots (statements) node
    (let ((first-statement (first statements))
          (last-statement  (first (last statements))))
      (make-instance 'span
                     :from (span-from first-statement)
                     :to (span-to last-statement)))))

;;; ===========================================================================
;;; Declarations
;;; ===========================================================================

(defclass declaration (statement) ())

(defclass bad-declaration (declaration)
  ((message :reader bad-declaration-message
            :initarg :message
            :initform (error "must provide message")
            :type string))
  (:documentation "A declaration that could not be parsed correctly."))

(defmethod children ((node bad-declaration))
  nil)

(defmethod location:span-for ((node bad-declaration))
  (with-slots (location) node
    (make-instance 'span
                   :from location
                   :to location)))

(defclass short-variable-declaration (declaration)
  ((identifiers
    :reader short-variable-declaration-identifiers
    :initarg :identifiers
    :initform (error "must provide variables")
    :type identifier-list
    :documentation "The list of variables")
   (expressions
    :reader short-variable-declaration-expressions
    :initarg :expressions
    :initform (error "must provide initializer")
    :type expression-list))
  (:documentation "The list of expressions for the variables"))

(defmethod children ((node short-variable-declaration))
  (list
   (short-variable-declaration-identifiers node)
   (short-variable-declaration-expressions node)))

(defmethod location:span-for ((node short-variable-declaration))
  (with-slots (identifiers expressions) node
    (let ((first-identifier (first identifiers))
          (last-expression  (first (last expressions))))
      (make-instance 'span
                     :from (token:location first-identifier)
                     :to (span-to last-expression)))))

;;; ===========================================================================
;;; Expressions
;;; ===========================================================================

(defclass expression (node) ()
  (:documentation "The base class for all expressions in the highlevel AST."))

(defclass literal (expression)
  ((token
    :reader literal-token
    :initarg :token
    :initform (error "must provide token")
    :type token:token))
  (:documentation "The base class for all literals in the highlevel AST."))

(defmethod children ((node literal))
  nil)

(defmethod location:span-for ((node literal))
  (with-slots (token) node
    (make-instance 'span
                   :from (token:location token)
                   :to (token:location token))))

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

(defmethod children ((node grouping-expression))
  (list (grouping-expression-expression node)))


(defmethod location:span-for ((node grouping-expression))
  (with-slots (expression) node
    (let ((sub-expr-span (span expression)))
      (make-instance 'span
                     :from (span-from sub-expr-span)
                     :to (span-to sub-expr-span)))))


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

(defmethod children ((node unary-expression))
  (list (unary-expression-operand node)))

(defmethod location:span-for ((node unary-expression))
  (with-slots (operator operand) node
    (let ((operand (span operand)))
      (make-instance 'span
                     :from (token:location operator)
                     :to (span-to operand)))))


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

(defmethod children ((node binary-expression))
  (list (binary-expression-lhs node)
        (binary-expression-rhs node)))

(defmethod location:span-for ((node binary-expression))
  (with-slots (lhs rhs) node
    (let ((lhs (location:span-for lhs))
          (rhs (location:span-for rhs)))
      (make-instance 'span
                     :from (span-from lhs)
                     :to (span-to rhs)))))

(defclass expression-list (expression)
  ((expressions
    :reader expression-list-expressions
    :initarg :expressions
    :initform (error "must provide expressions")
    :type list))
  (:documentation "An expression that is a list of expressions"))

(defmethod children ((node expression-list))
  (expression-list-expressions node))

(defmethod location:span-for ((node expression-list))
  (with-slots (expressions) node
    (let ((first-expression (first expressions))
          (last-expression  (first (last expressions))))
      (make-instance 'span
                     :from (span-from first-expression)
                     :to (span-to last-expression)))))

(defclass variable (node)
  ((identifier
    :reader variable-identifier
    :initarg :identifier
    :initform (error "must provide identifier")
    :type token:token))
  (:documentation "The base class for all variables in the highlevel AST."))

(defmethod children ((node variable))
  nil)

(defmethod location:span-for ((node variable))
  (with-slots (identifier) node
    (make-instance 'span
                   :from (token:location identifier)
                   :to (token:location identifier))))

(defclass identifier (node)
  ((token
    :reader identifier-token
    :initarg :token
    :initform (error "must provide token")
    :type token:token))
  (:documentation "The base class for all identifiers in the highlevel AST."))

(defmethod children ((node identifier))
  nil)

(defmethod location:span-for ((node identifier))
  (with-slots (token) node
    (make-instance 'span
                   :from (token:location token)
                   :to (token:location token))))

(defun identifier-name (identifier)
  (token:lexeme (identifier-token identifier)))

(defclass identifier-list (node)
  ((identifiers
    :reader identifier-list-identifiers
    :initarg :identifiers
    :initform (error "must provide variables")
    :type list))
  (:documentation "A list of identifiers"))

(defmethod children ((node identifier-list))
  (identifier-list-identifiers node))

(defmethod location:span-for ((node identifier-list))
  (with-slots (identifiers) node
    (let ((first-identifier (first identifiers))
          (last-identifier  (first (last identifiers))))
      (make-instance 'span
                     :from (token:location first-identifier)
                     :to (token:location last-identifier)))))

