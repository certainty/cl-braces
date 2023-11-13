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
;; We chose a representation that is easy to decode and cache-friendly.
;; TODO: I'm not sure the tiered struct approach is the best way to go.
(defstruct instruction
  (opcode (error "must supply opcode") :type opcode-t :read-only t)
  (operands (error "must supply operands") :type (vector operand-t) :read-only t))

(defmethod print-object ((instruction instruction) stream)
  (print-unreadable-object (instruction stream :type nil)
    (format stream "INSTRUCTION OPCODE: ~A ~A" (instruction-opcode instruction) (instruction-operands instruction))))

(deftype constant-table () '(vector value:value))

(defstruct chunk
  ;; The constants table is a vector of runtiome:value. Constants can thus be access by index.
  (constants (error "must supply constants") :type (vector value:value) :read-only t)
  ;; The code is a vector of instructions, so we can implement jumps efficiently.
  (code (error "must supply code") :type (vector instruction) :read-only t)
  ;; The registers-used is a hint to the VM which sets up the call-frame for this chunk, to allocate the right amount of registers in the register file.
  (registers-used 0 :type (integer 0 *)  :read-only t))

(serapeum:define-do-macro do-instructions ((pc instruction chunk &optional return) &body body)
  (let ((code (gensym)))
    `(let ((,code (chunk-code ,chunk)))
       (loop for ,pc from 0 below (length ,code)
             do (let ((,instruction (aref ,code ,pc)))
                  ,@body)))))
