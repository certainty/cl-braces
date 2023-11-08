(in-package :cl-braces.compiler.backend.codegen)

(defclass constants-builder ()
  ((registered-constants :initform (make-hash-table :test 'equal))
   (next-index :initform 0))
  (:documentation "Build the constants segment for the generated chunk"))

(defclass chunk ()
  ((constants
    :reader chunk-constants
    :initarg :constants
    :initform (error "must supply constants"))
   (code
    :reader chunk-code
    :initarg :code
    :initform (error "must supply code"))
   (registers-used
    :reader registers-used
    :initarg :registers-used
    :initform (error "must supply registers used")
    :type (integer 0 *))))

(defclass chunk-builder ()
  ((constants
    :initform (make-instance 'constants-builder)
    :type constants-builder)
   (instructions
    :initform (make-array 0 :adjustable t :fill-pointer t)
    :type (simple-array instruction))
   (operands
    :initform nil
    :type list
    :documentation "The stack of operands we need to keep track of. This is filled when child nodes are processed and consumed when the parent nodes are generated")
   (register-counter ; the most simple register allocator ever
    :initform 0
    :type register
    :documentation "The next register to use for a temporary value"))
  (:documentation "Builds a chunk of bytecode"))

(defgeneric result (builder)
  (:documentation "Return the result of a builder"))

(defun make-chunk-builder ()
  (make-instance 'chunk-builder))

(-> next-register (chunk-builder) register)
(defun next-register (builder)
  "Get the next register to use"
  (with-slots (register-counter) builder
    (prog1 (reg register-counter)
      (incf register-counter))))

(-> add-constant (chunk-builder value) address)
(defun add-constant (builder constant)
  "Register a constant for this chunk. Returns the address of the constant"
  (with-slots (constants) builder
    (with-slots (registered-constants next-index) constants
      (let ((index (gethash constant registered-constants)))
        (if index
            (addr index)
            (let ((idx next-index))
              (incf next-index)
              (setf (gethash constant registered-constants) idx)
              (addr idx)))))))

(defun generate-chunk (ast)
  (let ((builder (make-chunk-builder)))
    (emit builder ast)
    (result builder)))

(defmethod result ((builder constants-builder))
  (with-slots (registered-constants) builder
    (let* ((constants registered-constants)
           (constant-table (make-array (hash-table-count constants))))
      (loop for constant being the hash-key using (hash-value adress) of constants
            do
               (setf (aref constant-table adress) constant)
            finally (return constant-table)))))

(defmethod result ((builder chunk-builder))
  (with-slots (constants instructions register-counter) builder
    (bytecode:make-chunk :constants (result constants) :code instructions :registers-used register-counter)))

(defgeneric emit (backend node)
  (:documentation "Emit code for a given ast node for a particular backend"))

(defmethod emit ((builder chunk-builder) node)
  (ast:with-postorder-traversal
    (ast:walk builder node)))

(defmethod ast:enter ((builder chunk-builder) (node ast:literal))
  (with-slots (operands) builder
    (let ((const-address (add-constant builder (ast:literal-value node)))
          (register (next-register builder)))
      (push register operands)
      (add-instructions builder (bytecode:instr 'bytecode:loada register const-address)))))

(defmethod ast:enter ((builder chunk-builder) (node ast:grouping-expression))
  (emit builder (ast:grouping-expression-expression node)))

(defmethod ast:enter ((builder chunk-builder) (node ast:unary-expression))
  (with-slots (operands) builder
    (let ((op (ast:unary-expression-operator node))
          (operand (car (last operands))))
      (assert operand)
      (cond
        ((token:class= op token:@MINUS)
         (add-instructions builder (bytecode:instr 'bytecode:neg operand)))
                                        ; no-op
        ((token:class= op token:@PLUS) nil)
        (t (todo! "unary operator"))))))

(defmethod ast:enter ((builder chunk-builder) (node ast:binary-expression))
  (with-slots (operands) builder
    (let ((right (pop operands))
          (left (pop operands))
          ;; should we use an accumulator register?
          (dst (next-register builder))
          (op (ast:binary-expression-operator node)))
      (assert left)
      (assert right)
      (push dst operands)
      (cond
        ((token:class= op token:@PLUS)
         (add-instructions builder (bytecode:instr 'bytecode:add dst left right)))
        ((token:class= op token:@MINUS)
         (add-instructions builder (bytecode:instr 'bytecode:sub dst left right)))
        ((token:class= op token:@STAR)
         (add-instructions builder (bytecode:instr 'bytecode:mul dst left right)))
        ((token:class= op token:@SLASH)
         (add-instructions builder (bytecode:instr 'bytecode:div dst left right)))
        (t (todo! "binary operator"))))))

(defun add-instructions (chunk-builder &rest instruction)
  "Add instructions to the chunk"
  (with-slots (instructions) chunk-builder
    (loop :for i :in instruction
          :do (vector-push-extend i instructions))))
