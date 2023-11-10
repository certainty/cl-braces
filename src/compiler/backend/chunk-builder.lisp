(in-package :cl-braces.compiler.backend.codegen)

(defclass chunk-builder ()
  ((constants
    :initform (make-constants-builder)
    :type constants-builder)
   (instructions
    :initform (make-array 0 :adjustable t :fill-pointer t)
    :type (simple-array instruction))
   (operands
    :initform nil
    :type list
    :documentation "The stack of operands we need to keep track of. This is filled when child nodes are processed and consumed when the parent nodes are generated"))
  (:documentation "Builds a chunk of bytecode"))

(defun make-chunk-builder ()
  (make-instance 'chunk-builder))

(-> add-constant (chunk-builder vm.value:value) bytecode:address)
(defun add-constant (builder constant)
  "Register a constant for this chunk. Returns the address of the constant"
  (with-slots (constants) builder
    (constants-add constants constant)))

(defun add-instructions (builder instruction &rest more-instructions)
  "Add an instruction to the chunk"
  (with-slots (instructions) builder
    (vector-push-extend instruction instructions)
    (dolist (instr more-instructions)
      (vector-push-extend instr instructions))))

(defmethod chunk-result ((builder chunk-builder) registers-used)
  (with-slots (constants instructions) builder
    (bytecode:make-chunk
     :constants (constants-result constants)
     :code instructions
     :registers-used registers-used)))
