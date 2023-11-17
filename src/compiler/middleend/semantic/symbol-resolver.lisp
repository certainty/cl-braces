(in-package :cl-braces.compiler.middleend.semantic.symbol-resolver)

;;;; this pass deals with symbol resolution and ultimately creates symbol-table(s)
;;;; to use for downstream passes
;;;;

;;;; however for now we start with a simple scope analysis to find references to undefined symbols
;;;;
;;;;

(define-condition semantic-error (error)
  ((location :initarg :location :reader location)))

(define-condition variable-already-defined (semantic-error)
  ((name :initarg :symbol :reader name)))

(define-condition undefined-symbol (semantic-error)
  ((name :initarg :symbol :reader name)))

(defclass resolver ()
  ((symbol-table
    :initform (symbols:make-symbol-table))
   (current-scope
    :initarg :current-scope
    :initform 0
    :type (integer 0 *))
   (errors
    :reader errors-of
    :initform nil)))

(defun make-resolver ()
  (make-instance 'resolver))

(-> make-resolver (ast:node) (values symbols:symbol-table list))
(defun resolve-symbols (ast)
  (let ((resolver (make-resolver)))
    (ast:with-preorder-traversal
      (ast:walk resolver ast))
    (with-slots (errors symbol-table) resolver
      (values symbol-table (when errors (nreverse errors))))))

(defmethod ast:enter ((resolver resolver) (node ast:node))
  :continue)

(defmethod ast:leave ((resolver resolver) (node ast:node))
  :continue)

(defmethod ast:enter ((resolver resolver) (node ast:block))
  (with-slots (current-scope) resolver
    (incf current-scope)))

(defmethod ast:leave ((resolver resolver) (node ast:block))
  (with-slots (current-scope) resolver
    (decf current-scope)))

(defmethod ast:enter ((resolver resolver) (node ast:short-variable-declaration))
  (with-slots (symbol-table current-scope errors) resolver
    (let* ((variable (ast:short-variable-declaration-variable node))
           (identifier (token:lexeme (ast:variable-identifier variable))))
      (if (symbols:find-by-name symbol-table identifier :denotation #'symbols:denotes-variable-p :scope<= current-scope)
          (push (make-condition 'variable-already-defined :symbol identifier :location (ast:location variable)) errors)
          (symbols:add-symbol symbol-table identifier :variable :scope current-scope :location (ast:location variable))))))

(defmethod ast:enter ((resolver resolver) (node ast:variable))
  (with-slots (current-scope errors symbol-table) resolver
    (let ((variable (token:lexeme (ast:variable-identifier node))))
      (unless (symbols:find-by-name symbol-table variable :denotation #'symbols:denotes-variable-p :scope<= current-scope)
        (push (make-condition 'undefined-symbol :symbol variable :location (ast:location node)) errors)))))
