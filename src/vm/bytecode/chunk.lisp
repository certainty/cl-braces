(in-package :cl-braces.vm.bytecode)

(deftype opcode-t ()  '(unsigned-byte 8))
(deftype register-t () '(unsigned-byte 16))
(deftype address-t () '(unsigned-byte 64))
(deftype operand-t () '(or register-t address-t))

;; A machine instruction as loaded and executed by the vm.
;; We chose a representation that is easy to decode and cache-friendly.
(defstruct instruction
  (opcode (error "must supply opcode") :type opcode-t :read-only t))

(defstruct (unary-instruction (:include instruction))
  (op1 (error "must supply op1") :type operand-t :read-only t))

(defstruct (binary-instruction (:include unary-instruction))
  (op2 (error "must supply op2") :type operand-t :read-only t))

(defstruct (ternary-instruction (:include binary-instruction))
  (op3 (error "must supply op3") :type operand-t :read-only t))

(deftype constant-table () '(vector value:value))

(defun instruction-operands (instruction)
  (cond
    ((ternary-instruction-p instruction)
     (vector (ternary-instruction-op1 instruction)
             (ternary-instruction-op2 instruction)
             (ternary-instruction-op3 instruction)))
    ((binary-instruction-p instruction)
     (vector (binary-instruction-op1 instruction)
             (binary-instruction-op2 instruction)))
    ((unary-instruction-p instruction)
     (vector (unary-instruction-op1 instruction)))))

(defstruct chunk
  (constants (error "must supply constants") :type (vector value:value) :read-only t)
  (code (error "must supply code") :type (vector instruction) :read-only t)
  (registers-used 0 :type (integer 0 *) :read-only t))

(serapeum:define-do-macro do-instructions ((pc instruction chunk &optional return) &body body)
  (let ((code (gensym)))
    `(let ((,code (chunk-code ,chunk)))
       (loop for ,pc from 0 below (length ,code)
             do (let ((,instruction (aref ,code ,pc)))
                  ,@body)))))
