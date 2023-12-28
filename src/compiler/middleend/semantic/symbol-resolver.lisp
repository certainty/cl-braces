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
   (context
    :initarg :context
    :initform nil
    :type (or null ast:node))
   (errors
    :reader errors-of
    :initform nil
    :type (support:list-of semantic-error))))

(-> make-resolver () resolver)
(defun make-resolver ()
  (let ((res (make-instance 'resolver)))
    res))

(defun resolve-symbols (ast &key fail-fast)
  (let ((resolver (make-resolver)))
    (with-slots (symbol-table errors) resolver
      (handler-bind ((semantic-error (lambda (c)
                                       (if fail-fast
                                           (invoke-debugger c)
                                           (when (find-restart 'continue)
                                             (push errors c)
                                             (invoke-restart 'continue))))))
        (ast:walk resolver ast)
        (values symbol-table (when errors (nreverse errors)))))))

(defmethod ast:enter ((resolver resolver) (node ast:node))
  (declare (ignore resolver node))
  :continue)

(defmethod ast:leave ((resolver resolver) (node ast:node))
  (declare (ignore resolver node))
  :continue)

(defmethod ast:enter ((resolver resolver) (tok token:token))
  (declare (ignore resolver tok))
  :continue)

(defmethod ast:leave ((resolver resolver) (tok token:token))
  (declare (ignore resolver tok))
  :continue)

(defmethod ast:enter ((resolver resolver) (node ast:block))
  (enter-scope resolver))

(defmethod ast:leave ((resolver resolver) (node ast:block))
  (leave-scope resolver))

(defun enter-scope (resolver)
  (with-slots (current-scope) resolver
    (incf current-scope)))

(defun leave-scope (resolver)
  (with-slots (current-scope) resolver
    (decf current-scope)))

(defmacro with-new-scope (resolver &body body)
  `(unwind-protect
        (progn
          (enter-scope ,resolver)
          ,@body)
     (leave-scope ,resolver)))

(defmethod ast:enter ((resolver resolver) (node ast:short-variable-declaration))
  (with-slots (symbol-table current-scope errors) resolver
    (let* ((variables (ast:short-variable-declaration-identifiers node)))
      (dolist (variable (ast:identifier-list-identifiers variables))
        (let ((identifier (ast:identifier-name variable)))
          (a:if-let ((existing (symbols:find-by-name symbol-table identifier :denotation #'symbols:denotes-variable-p :scope<= current-scope)))
            (dolist (existing existing)
              (unless (symbols:place-holder-p existing)
                (cerror "Variable already defined" (make-condition 'variable-already-defined :symbol identifier :location (ast:location variable)))))
            (symbols:add-symbol symbol-table identifier :variable :scope current-scope :location (ast:location variable))))))))

(defmethod ast:enter ((resolver resolver) (node ast:identifier))
  (with-slots (current-scope errors symbol-table) resolver
    (let ((variable (ast:identifier-name node)))
      (unless
          (or (symbols:find-by-name symbol-table variable :denotation #'symbols:denotes-variable-p :scope<= current-scope)
              (symbols:find-by-name symbol-table variable :denotation #'symbols:denotes-function-p :scope<= 0))
        (cerror "Undefined symbol" (make-condition 'undefined-symbol :symbol variable :location (ast:location node)))))))

(defmethod ast:enter ((resolver resolver) (node ast:variable-specification))
  (with-slots (symbol-table current-scope errors) resolver
    (let* ((variables (ast:variable-specification-identifiers node)))
      (dolist (identifier (ast:identifier-list-identifiers variables))
        (let ((name (ast:identifier-name identifier)))
          (a:if-let ((existing (symbols:find-by-name symbol-table name :denotation #'symbols:denotes-variable-p :scope<= current-scope)))
            (dolist (existing existing)
              (unless (symbols:place-holder-p existing)
                (cerror "Variable already defined" (make-condition 'variable-already-defined :symbol name :location (ast:location identifier)))))
            (symbols:add-symbol symbol-table name :variable :scope current-scope :location (ast:location identifier))))))))

(defmethod ast:enter ((resolver resolver) (node ast:function-declaration))
  (with-slots (symbol-table current-scope errors) resolver
    (let ((name (ast:identifier-name (ast:function-declaration-name node))))
      (when (symbols:find-by-name symbol-table name :denotation #'symbols:denotes-function-p)
        (cerror "Function already defined" (make-condition 'variable-already-defined :symbol name :location (ast:location node))))
      (symbols:add-symbol symbol-table name :function :scope 0 :location (ast:location node)))))

(defmethod ast:enter ((resolver resolver) (node ast:function-signature))
  (enter-scope resolver))

(defmethod ast:leave ((resolver resolver) (node ast:function-signature))
  (leave-scope resolver))

(defmethod ast:enter ((resolver resolver) (node ast:block))
  (enter-scope resolver))

(defmethod ast:leave ((resolver resolver) (node ast:block))
  (leave-scope resolver))

(defmethod ast:enter ((resolver resolver) (node ast:parameter-declaration))
  (with-slots (symbol-table current-scope errors) resolver
    (dolist (parameter (ast:identifier-list-identifiers (ast:parameter-declaration-identifiers node)))
      (let ((name (ast:identifier-name parameter)))
        (a:if-let ((exising (symbols:find-by-name symbol-table name :denotation #'symbols:denotes-variable-p :scope<= current-scope)))
          (cerror "Variable already defined" (make-condition 'variable-already-defined :symbol name :location (ast:location parameter)))
          (symbols:add-symbol symbol-table name :variable :scope current-scope :location (ast:location parameter)))))))
