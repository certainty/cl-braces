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
    (bytecode:reg (incf next-register))))

(defmethod registers-used ((allocator register-allocator))
  (with-slots (next-register) allocator
    (1+ next-register)))

(defun make-register-allocator ()
  (make-instance 'register-allocator))

;;; Scoped variable management
;;; TODO: explain how this works
;;;
(defclass variable-manager ()
  ((variables :initform (make-hash-table :test 'equal))))

(defun make-variable-manager ()
  (make-instance 'variable-manager))

(deftype scope-t () '(integer 0 *))

(-> resolve-variable (variable-manager string scope-t) (or null bytecode:register))
(defun resolve-variable (manager name scope)
  "Find a variable with the given name and scope.
The scope is simply a number from 0 to N where 0 is the global `scope', 1 is the next lexical scope etc.
The function returns the `bytecode:register' of the variable with that name closest to the given scope.
If no variable with that name can be found it returns NIL.
 "
  (with-slots (variables) manager
    (when-let ((scopes (gethash name variables)))
      ;; scopes is an alist which maps scopes to register
      ;; a scope is simply a number from 0 to N
      ;; where 0 is the global scope, 1 is the next lexical scope etc.
      ;; now we simply have to check if a variable has been defined with that
      ;; name and and return the closest one in terms of scope
      (loop for next from scope downto 0
            for candidate = (assoc next scopes)
            when candidate
              return (cdr candidate)))))

(-> add-variable (variable-manager string scope-t bytecode:register) list)
(defun add-variable (manager name scope register)
  "Add a variable with the given name and scope to the manager."
  (with-slots (variables) manager
    (if (gethash name variables)
        (push (cons scope register) (gethash name variables))
        (setf (gethash name variables) (list (cons scope register))))))

(defclass bytecode-generator ()
  ((chunk-builder
    :initform (make-chunk-builder))
   (operands
    :initform nil)
   (variables
    :initform (make-variable-manager))
   (current-scope
    :initform 0
    :type scope-t)
   (register-allocator
    :initform (make-register-allocator))))

(defun make-bytecode-generator ()
  (make-instance 'bytecode-generator))

(defun enter-scope (generator)
  (with-slots (current-scope) generator
    (incf current-scope)))

(defun leave-scope (generator)
  (with-slots (current-scope) generator
    (decf current-scope)))

(defun find-scoped-variabel (generator name)
  (with-slots (current-scope variables) generator
    (resolve-variable variables name current-scope)))

(defun create-scoped-variable (generator name)
  (with-slots (current-scope variables register-allocator) generator
    (let ((var (next-register register-allocator)))
      (prog1 var
        (add-variable variables name current-scope var)))))

(defun find-or-create-scoped-variable (generator name)
  (or (find-scoped-variabel generator name)
      (create-scoped-variable generator name)))

(defun generate-chunk (ast)
  (let ((generator (make-bytecode-generator)))
    (generate generator ast)
    (with-slots (chunk-builder register-allocator) generator
      (chunk-result chunk-builder (registers-used register-allocator)))))

(defgeneric generate (generator node)
  (:documentation "Emit code for a given ast node for a particular backend"))

(defmethod generate ((generator bytecode-generator) node)
  (ast:with-postorder-traversal
    (ast:walk generator node)))

(defmethod ast:enter ((generator bytecode-generator) (node ast:program))
  :continue)

(defmethod ast:enter ((generator bytecode-generator) (node ast:literal))
  (with-slots (operands chunk-builder register-allocator) generator
    (let ((const-address (add-constant chunk-builder (value:box (ast:literal-value node))))
          (register (next-register register-allocator)))
      (push register operands)
      (add-instructions chunk-builder (bytecode:instr 'bytecode:loada register const-address)))))

(defmethod ast:enter ((generator bytecode-generator) (node ast:grouping-expression))
  ;; nothing to do
  :continue)

(defmethod ast:enter ((generator bytecode-generator) (node ast:unary-expression))
  (with-slots (operands chunk-builder) generator
    (let ((op (ast:unary-expression-operator node))
          (operand (car (last operands))))
      (assert operand)
      (cond
        ((token:class= op token:@MINUS)
         (add-instructions chunk-builder (bytecode:instr 'bytecode:neg operand)))
                                        ; no-op
        ((token:class= op token:@PLUS) nil)
        (t (todo! "unary operator"))))))

(defmethod ast:enter ((generator bytecode-generator) (node ast:binary-expression))
  (with-slots (operands chunk-builder register-allocator) generator
    (let* ((right (pop operands))
           (left (pop operands))
           ;; should we use an accumulator register?
           (dst (next-register register-allocator))
           (op (ast:binary-expression-operator node)))
      (assert left)
      (assert right)
      (push dst operands)
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

(defmethod ast:enter ((generator bytecode-generator) (node ast:expression-statement))
  :continue)

(defmethod ast:enter ((generator bytecode-generator) (node ast:variable))
  (with-slots (operands) generator
    (let ((variable-name (token:lexeme (ast:variable-identifier node))))
      (push (find-or-create-scoped-variable generator variable-name) operands))))

(defmethod ast:enter ((generator bytecode-generator) (node ast:short-variable-declaration))
  (with-slots (operands chunk-builder) generator
    (let ((initializer-result-reg (pop operands))
          (var-reg (pop operands)))
      (assert var-reg)
      (assert initializer-result-reg)
      (add-instructions chunk-builder (bytecode:instr 'bytecode:mov var-reg initializer-result-reg)))))
