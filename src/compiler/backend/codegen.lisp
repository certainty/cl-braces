(in-package :cl-braces.compiler.backend.codegen)

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

(defclass bytecode-generator ()
  ((chunk-builder
    :initform (make-chunk-builder))
   (operands
    :initform nil)
   (register-allocator
    :initform (make-register-allocator))))

(defun make-bytecode-generator ()
  (make-instance 'bytecode-generator))

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
    (let ((right (pop operands))
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
