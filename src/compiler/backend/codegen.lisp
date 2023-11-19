(in-package :cl-braces.compiler.backend.codegen)
;;;; Code generation
;;;; TODO: explain how this works

;;; Register allocation
;;; TODO: explain how this works
;;;
(defclass register-allocator ()
  ((next-register
    :initform -1)))

(defgeneric next-register (allocator)
  (:documentation "Get the next register to use"))

(defmethod next-register ((allocator register-allocator))
  (with-slots (next-register) allocator
    (bytecode:register (incf next-register))))

(defmethod registers-used ((allocator register-allocator))
  (with-slots (next-register) allocator
    (1+ next-register)))

(defun make-register-allocator ()
  (make-instance 'register-allocator))

(defclass bytecode-generator ()
  ((symbol-table
    :initarg :symbol-table
    :initform (error "No symbol table specified"))
   (chunk-builder
    :initform (make-chunk-builder))
   (variable-registers
    :initform (make-hash-table))
   (current-scope
    :initform 0
    :type scope-t)
   (operand-stack
    :initform nil)
   (register-allocator
    :initform (make-register-allocator))))

(defun make-bytecode-generator (symbol-table)
  (make-instance 'bytecode-generator :symbol-table symbol-table))

(defun enter-scope (generator)
  (with-slots (current-scope) generator
    (incf current-scope)))

(defun leave-scope (generator)
  (with-slots (current-scope) generator
    (decf current-scope)))

(defun find-scoped-variable (generator name)
  (with-slots (current-scope variable-registers symbol-table) generator
    (a:when-let ((var (symbols:closest-scope current-scope (symbols:find-by-name symbol-table name :denotation #'symbols:denotes-variable-p))))
      (symbols:id var))))

(defun find-register-for (generator variable-id)
  (with-slots (variable-registers) generator
    (gethash variable-id variable-registers)))

(defun create-register-for (generator variable-id)
  (with-slots (variable-registers register-allocator) generator
    (let ((reg (next-register register-allocator)))
      (prog1 reg
        (setf (gethash variable-id variable-registers) reg)))))

(defun generate-chunk (ast symbol-table)
  (let ((generator (make-bytecode-generator symbol-table)))
    (generate generator ast)
    (with-slots (chunk-builder register-allocator) generator
      (chunk-result chunk-builder (registers-used register-allocator)))))

;; At a later point, when we only generate from simplified code in SSA we can switch to using ast:walk.
;; For now we implement the traversal ourselves to have full control
(defgeneric generate (generator node)
  (:documentation "Generate code for a node"))

(defmethod generate ((generator bytecode-generator) (node ast:node))
  (declare (ignore node generator))
  t)

(defmethod generate ((generator bytecode-generator) (node ast:program))
  (with-slots (chunk-builder) generator
    (let ((statements (ast:program-declarations node)))
      (generate generator statements))))

(defmethod generate ((generator bytecode-generator) (node ast:statement-list))
  (with-slots (chunk-builder) generator
    (let ((statements (ast:statement-list-statements node)))
      (loop for statement in statements
            for stmt-reg = (generate generator statement)
            finally (return stmt-reg)))))

(defmethod generate ((generator bytecode-generator) (node ast:expression-list))
  (with-slots (chunk-builder) generator
    (let ((expressions (ast:expression-list-expressions node)))
      (loop for expression in expressions
            for expr-reg = (generate generator expression)
            finally (return expr-reg)))))

(defmethod generate ((generator bytecode-generator) (node ast:expression-statement))
  (with-slots (chunk-builder) generator
    (let ((expression (ast:expression-statement-expression node)))
      (generate generator expression))))

(defmethod generate ((generator bytecode-generator) (node ast:literal))
  (with-slots (chunk-builder register-allocator) generator
    (s:lret* ((const-address (add-constant chunk-builder (value:box (ast:literal-value node))))
              (register (next-register register-allocator)))
      (add-instructions chunk-builder (bytecode:instr 'bytecode:loada register const-address)))))

(defmethod generate ((generator bytecode-generator) (node ast:unary-expression))
  (with-slots (chunk-builder register-allocator) generator
    (s:lret ((op (ast:unary-expression-operator node))
             (operand (generate generator (ast:unary-expression-operand node))))
      (assert operand)
      (cond
        ((token:class= op token:@MINUS)
         (add-instructions chunk-builder (bytecode:instr 'bytecode:neg operand)))
        ((token:class= op token:@PLUS) t)
        (t (todo! "unary operator"))))))

(defmethod generate ((generator bytecode-generator) (node ast:binary-expression))
  (with-slots (chunk-builder register-allocator) generator
    (s:lret* ((op (ast:binary-expression-operator node))
              (left (generate generator (ast:binary-expression-lhs node)))
              (right (generate generator (ast:binary-expression-rhs node)))
              (dst (next-register register-allocator)))
      (assert left)
      (assert right)
      (cond
        ((token:class= op token:@PLUS)
         (add-instructions chunk-builder (bytecode:instr 'bytecode:add dst left right)))
        ((token:class= op token:@MINUS)
         (add-instructions chunk-builder (bytecode:instr 'bytecode:sub dst left right)))
        ((token:class= op token:@STAR)
         (add-instructions chunk-builder (bytecode:instr 'bytecode:mul dst left right)))
        ((token:class= op token:@SLASH)
         (add-instructions chunk-builder (bytecode:instr 'bytecode:div dst left right)))
        (t (todo! "binary operator"))))))

(defmethod generate ((generator bytecode-generator) (node ast:short-variable-declaration))
  (with-slots (chunk-builder register-allocator) generator
    (let* ((expression-list (ast:short-variable-declaration-expressions node))
           (identifier-list (ast:short-variable-declaration-identifiers node)))
      (loop for src-reg in (generate generator expression-list)
            for dst-reg in (registers-for-identifiers generator identifier-list :create-if-missing t)
            do (add-instructions chunk-builder (bytecode:instr 'bytecode:mov dst-reg src-reg))
            finally (return dst-reg)))))

(defmethod generate ((generator bytecode-generator) (node ast:expression-list))
  (with-slots (chunk-builder register-allocator) generator
    (let ((expressions (ast:expression-list-expressions node)))
      (loop for expression in expressions
            for reg = (generate generator expression)
            collect reg))))

(defun registers-for-identifiers (generator identifier-list &key (create-if-missing nil))
  (with-slots (variable-registers) generator
    (loop for identifier in (ast:identifier-list-identifiers identifier-list)
          for id = (find-scoped-variable generator (ast:identifier-name identifier))
          for reg = (find-register-for generator id)
          do (when (and create-if-missing (null reg))
               (setf reg (create-register-for generator id)))
          collect reg)))

(defmethod generate ((generator bytecode-generator) (node ast:identifier))
  (let ((name (ast:identifier-name node)))
    (a:when-let ((variable-id (find-scoped-variable generator name)))
      (find-register-for generator variable-id))))

(defmethod generate ((generator bytecode-generator) (node ast:empty-statement))
  (declare (ignore node generator))
  t)

(defmethod generate ((generator bytecode-generator) (node ast:if-statement))
  (with-slots (chunk-builder) generator
    (let* ((init-stmt (ast:if-statement-init node))
           (condition (ast:if-statement-condition node))
           (condition-reg nil)
           (consequence (ast:if-statement-consequence node))
           (alternative (ast:if-statement-alternative node))
           (br-addr 0)
           (end-addr 0)
           (label-alternative nil)
           (label-end nil))

      (generate generator init-stmt)
      (setf condition-reg (generate generator condition))

      (setf br-addr (add-instructions chunk-builder (bytecode:instr 'bytecode:br condition-reg #xDEADBEEF #xDEADBEEF)))

      (generate generator consequence)
      (setf end-addr (add-instructions chunk-builder (bytecode:instr 'bytecode:jmp #xDEADBEEF)))

      (when alternative
        ;; jump over the alternative
        (setf label-alternative (add-label chunk-builder "if-alternative"))
        (generate generator alternative)
        (setf end-addr (add-instructions chunk-builder (bytecode:instr 'bytecode:jmp #xDEADBEEF))))

      (setf label-end (add-label chunk-builder "if-end"))
      ;; patch up the addresses
      )))

(defmethod generate ((generator bytecode-generator) (node ast:block))
  (with-slots (chunk-builder) generator
    (let ((statements (ast:block-statements node)))
      (generate generator statements))))
