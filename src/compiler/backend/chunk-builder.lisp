(in-package :cl-braces.compiler.backend.codegen)

;;;; The chunk builder constructs the chunk of bytecode for the virtual machine machine.
;;;; Its main inteface is to add instructions and constants. See also the `constants-builder' for details.

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
    :documentation "The stack of operands we need to keep track of. This is filled when child nodes are processed and consumed when the parent nodes are generated")
   (blocklabels
    :initform (make-hash-table :test 'equal)
    :type hash-table))
  (:documentation "Builds a chunk of bytecode"))

(defun make-chunk-builder ()
  "Create a new chunk builder"
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
      (vector-push-extend instr instructions))
    (1- (length instructions))))

(defun replace-instruction (builder address instruction)
  "Replace an instruction at the given address"
  (with-slots (instructions) builder
    (setf (aref instructions address) instruction)))

(defun add-label (builder prefix)
  "Creates a label which points to the next instruction. Returns the address of the label"
  (with-slots (blocklabels instructions) builder
    (s:lret* ((label-name (format nil "~A" (gensym prefix)))
              (addr (length instructions))
              (label (bytecode:label addr)))
      (setf (gethash addr blocklabels) label-name))))

(defun chunk-result (builder registers-used)
  "Return the chunk of bytecode. All constants will be deduplicated."
  (with-slots (constants instructions blocklabels) builder
    (bytecode:chunk
     (constants-result constants)
     blocklabels
     instructions
     registers-used)))
