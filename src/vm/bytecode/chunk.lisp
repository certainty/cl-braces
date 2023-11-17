(in-package :cl-braces.vm.bytecode)

;;;; Here we define the in-memory representation of the bytecode instructions.
;;;; The objectvie here is use a representation that is cache friendly and easy to decode.
;;;;
;;;; The main artifact that exposed by thie is what we call a `chunk'.
;;;; A `chunk' is a collection of instructions and constants that are loaded into the VM.
;;;; This doesn't make any assumptions about how the compiler choses to reprsent the
;;;; compiled program. It could generate a chunk for the whole propgram or follow a chunk per function approach.

(deftype opcode-t ()  '(unsigned-byte 8))
(deftype register-t () '(unsigned-byte 16))
(deftype address-t () '(unsigned-byte 64))
(deftype operand-t () '(or register-t address-t))

;; A machine instruction as loaded and executed by the vm.
(serapeum:defconstructor instruction
  (opcode opcode-t)
  (operands (vector operand-t)))

(defmethod print-object ((instruction instruction) stream)
  (print-unreadable-object (instruction stream :type nil)
    (format stream "INSTRUCTION OPCODE: ~A ~A" (instruction-opcode instruction) (instruction-operands instruction))))

(deftype constant-table () '(vector value:value))

(serapeum:defconstructor chunk
  (constants constant-table)
  (code (vector instruction))
  (registers-used (integer 0 *)))

(serapeum:define-do-macro do-instructions ((pc instruction chunk &optional return) &body body)
  (let ((code (gensym)))
    `(let ((,code (chunk-code ,chunk)))
       (loop for ,pc from 0 below (length ,code)
             do (let ((,instruction (aref ,code ,pc)))
                  ,@body)))))
