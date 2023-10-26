(in-package :cl-braces.compiler.frontend.ast)

(defparameter *node-id-counter* 1 "Counter for AST node IDs. This is used to assign unique IDs to AST nodes.")

(defun next-node-id ()
  (prog1 *node-id-counter*
    (incf *node-id-counter*)))

(defstruct (node (:conc-name node-))
  (id (next-node-id) :type positive-fixnum :read-only t)
  (location (error "must provide location") :type scanner:source-location :read-only t))

(defstruct (expression (:include node)))

(defstruct (bad-expression (:include expression)))

(defstruct (literal-expression (:conc-name literal-exp-) (:include expression))
  (token (error "must provide token") :type scanner:token :read-only t))

(defstruct (identifier (:conc-name identifier-) (:include expression))
  (name (error "must provide token") :type string :read-only t))

(defstruct (statement (:include node)))

(defstruct (bad-statement (:include statement)))

(defstruct (declaration (:include node)))

(defstruct (const-declaration (:conc-name const-decl-) (:include declaration))
  (name (error "must provide name") :type identifier :read-only t)
  (initializer (error "must provide initializer") :type literal-expression :read-only t))

(defstruct (bad-declaration (:include declaration)))

(defstruct (source (:conc-name source-) (:include node))
  (declarations (error "must provide declarations") :type list :read-only t))
