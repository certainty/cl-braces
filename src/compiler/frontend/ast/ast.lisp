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

(defmethod walk (visitor (tok frontend.token:token))
  (enter visitor tok)
  (leave visitor tok))

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

(defmethod span:span-for ((node program))
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

(defmethod span:span-for ((node bad-statement))
  (with-slots (location) node
    (make-instance 'span
                   :from location
                   :to location)))

(defclass empty-statement (statement) ())

(defmethod children ((node empty-statement))
  nil)

(defmethod span:span-for ((node empty-statement))
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

(defmethod span:span-for ((node statement-list))
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

(defmethod span:span-for ((node expression-statement))
  (with-slots (expression) node
    (span:span-for expression)))

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

(defmethod span:span-for ((node if-statement))
  (with-slots (condition consequence alternative) node
    (let ((condition (span:span-for condition))
          (consequence (span:span-for consequence))
          (alternative (span:span-for alternative)))
      (make-instance 'span
                     :from (span-from condition)
                     :to (span-to (or alternative consequence))))))

(defclass assignment-statement (statement)
  ((lhs
    :reader assignment-statement-lhs
    :initarg :lhs
    :initform (error "must provide lhs")
    :type expression-list)
   (operator
    :reader assignment-statement-operator
    :initarg :operator
    :initform (error "must provide op")
    :type frontend.token:token)
   (rhs
    :reader assignment-statement-rhs
    :initarg :rhs
    :initform (error "must provide rhs")
    :type expression-list))
  (:documentation "A statement that is an assignment."))

(defmethod children ((node assignment-statement))
  (list (assignment-statement-lhs node)
        (assignment-statement-operator node)
        (assignment-statement-rhs node)))

(defmethod span:span-for ((node assignment-statement))
  (with-slots (lhs op rhs) node
    (let ((lhs (span:span-for lhs))
          (rhs (span:span-for rhs)))
      (make-instance 'span
                     :from (span-from lhs)
                     :to (span-to rhs)))))

(defclass block (node)
  ((statements
    :reader block-statements
    :initarg :statements
    :initform (error "must provide statements")
    :type statement-list))
  (:documentation "A block of statements"))

(defmethod children ((node block))
  (list (block-statements node)))

(defmethod span:span-for ((node block))
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

(defmethod span:span-for ((node bad-declaration))
  (with-slots (location) node
    (make-instance 'span
                   :from location
                   :to location)))

(defclass variable-declaration (declaration)
  ((specifications
    :reader variable-declaration-specifications
    :initarg :specifications
    :initform (error "must provide specifications")
    :type (support:list-of variable-specification)))
  (:documentation "A declaration that declares variables."))

(defmethod children ((node variable-declaration))
  (variable-declaration-specifications node))

(defmethod span:span-for ((node variable-declaration))
  (with-slots (specifications) node
    (make-instance 'span
                   :from (span-from specifications)
                   :to (span-to speicfications))))

(defclass variable-specification (declaration)
  ((identifiers
    :reader variable-specification-identifiers
    :initarg :identifiers
    :initform (error "must provide variables")
    :type (or identifier-list expression-list)
    :documentation "The list of variables to declar")
   (type
    :reader variable-specification-type
    :initarg :type
    :initform nil
    :type (or null type-specifier)
    :documentation "The type of the variables. If this is nil it means the type is inferred from the initializer.")
   (initializer
    :reader variable-specification-initializer
    :initarg :initializer
    :initform nil
    :type (or null expression-list)
    :documentation "The initializer for the variables")))

(defmethod children ((node variable-specification))
  (let ((base (list (variable-specification-identifiers node))))
    (when (variable-specification-type node)
      (push (variable-specification-type node) base))
    (when (variable-specification-initializer node)
      (push (variable-specification-initializer node) base))
    (reverse base)))

(defmethod span:span-for ((node variable-specification))
  (with-slots (identifiers initializer) node
    (let ((first-identifier (first identifiers))
          (type (variable-specification-type node))
          (last-initializer  (first (last initializer))))
      (make-instance 'span
                     :from (token:location first-identifier)
                     :to
                     (or
                      (and last-initializer (span-to last-initializer))
                      (and type (span-to type))
                      (token:location first-identifier))))))

(defclass type-specifier (node)
  ((name
    :reader type-specifier-name
    :initarg :name
    :initform (error "must provide name")
    :type frontend.token:token))
  (:documentation "The base class for all type specifiers in the highlevel AST."))

(defmethod children ((node type-specifier))
  (list (type-specifier-name node)))

(defmethod span:span-for ((node type-specifier))
  (with-slots (name) node
    (make-instance 'span
                   :from (token:location name)
                   :to (token:location name))))

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

(defmethod span:span-for ((node short-variable-declaration))
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
    :type frontend.token:token))
  (:documentation "The base class for all literals in the highlevel AST."))

(defmethod children ((node literal))
  (list (literal-token node)))

(defmethod span:span-for ((node literal))
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


(defmethod span:span-for ((node grouping-expression))
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
    :type frontend.token:token)
   (operand
    :reader unary-expression-operand
    :initarg :operand
    :initform (error "must provide operand")
    :type expression))
  (:documentation "An expression for binary relations"))

(defmethod children ((node unary-expression))
  (list
   (unary-expression-operator node)
   (unary-expression-operand node)))

(defmethod span:span-for ((node unary-expression))
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
    :type frontend.token:token)
   (rhs
    :reader binary-expression-rhs
    :initarg :rhs
    :initform (error "must provide rhs")
    :type expression))
  (:documentation "An expression for binary relations"))

(defmethod children ((node binary-expression))
  (list
   (binary-expression-operator node)
   (binary-expression-lhs node)
   (binary-expression-rhs node)))

(defmethod span:span-for ((node binary-expression))
  (with-slots (lhs rhs) node
    (let ((lhs (span:span-for lhs))
          (rhs (span:span-for rhs)))
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

(defmethod span:span-for ((node expression-list))
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
    :type frontend.token:token))
  (:documentation "The base class for all variables in the highlevel AST."))

(defmethod children ((node variable))
  (list (variable-identifier node)))

(defmethod span:span-for ((node variable))
  (with-slots (identifier) node
    (make-instance 'span
                   :from (token:location identifier)
                   :to (token:location identifier))))

(defclass identifier (node)
  ((token
    :reader identifier-token
    :initarg :token
    :initform (error "must provide token")
    :type frontend.token:token))
  (:documentation "The base class for all identifiers in the highlevel AST."))

(defmethod children ((node identifier))
  (list (identifier-token node)))

(defmethod span:span-for ((node identifier))
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

(defmethod span:span-for ((node identifier-list))
  (with-slots (identifiers) node
    (let ((first-identifier (first identifiers))
          (last-identifier  (first (last identifiers))))
      (make-instance 'span
                     :from (token:location first-identifier)
                     :to (token:location last-identifier)))))

