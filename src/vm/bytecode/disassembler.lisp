(in-package :cl-braces.vm.bytecode)

;;;; The disassembler gives insights into the encoded instructions.
;;;; This is useful for debugging purposes but also during development of the compiler and the VM itself.
;;;;
;;;; The main entry point is the `disass' function which takes a chunk of bytecode and prints it to the provided stream.
;;;; Example:
;;;;
;;;; ```common-lisp
;;;; (bytecode:disass (compiler:compile-this "3 + 4"))
;;;; ```

(defun disass (chunk &key (isa *current-isa*) (stream *standard-output*))
  "Disassemble a chunk of bytecode to the provided `stream'.
   The format of the disassembly is as follows:
   PC: [LABEL] ENCODED-INSTRUCTION OPCODE OPERANDS [COMMENT] "
  (do-instructions (pc instr chunk)
    (format stream "~3a: ~a ~8,a ~8,a ~15,a // ~a~%"
            (column-pc pc)
            (column-label instr isa chunk)
            (column-encoded-instruction instr isa)
            (column-opcode instr isa)
            (column-operands instr isa)
            (column-comment instr isa chunk))))

(defun column-pc (pc)
  (format nil "~3,'0X" pc))

(defun column-label (instr isa ch)
  "Prints the label for the given instruction."
  (declare (ignore instr isa ch))
  "")

(defun column-encoded-instruction (instr isa)
  "Return the instruction it its encoded form. It's opcode followed by operands"
  (declare (ignore isa))
  (with-output-to-string (stream)
    (let ((opcode (instruction-opcode instr))
          (operands (instruction-operands instr)))
      (format stream "~2,'0X" opcode)
      (loop for operand across operands
            do (format stream "~2,'0X" operand)))))

(defun column-opcode (instr isa)
  "Return the mnemonic for the given instruction."
  (let* ((opcode (instruction-opcode instr))
         (isa-instr (instruction-by-opcode opcode isa)))
    (assert isa-instr)
    (format nil "~a" (isa-instruction-mnemonic isa-instr))))

(defun column-operands (instr isa)
  "Returns the operands for the given instruction"
  (let* ((operand-values (instruction-operands instr))
         (isa-instr (instruction-by-opcode (instruction-opcode instr) isa))
         (isa-operands (isa-instruction-operands isa-instr)))
    (format nil "~{~a~^, ~}" (loop :for isa-op :across isa-operands
                                   :for op :across operand-values
                                   :collect (format-operand op (isa-operand-type isa-op))))))

(defun format-operand (value op-type)
  "Formats the given operand value according to the given operand type.
   Registers are formatted as $<reg-number>, addresses as @<address-number>."
  (cond
    ((eq op-type 'register) (format nil "$~a" value))
    ((eq op-type 'address) (format nil "@~a" value))
    (t (unreachable! "Unknown operand type"))))

(defun column-comment (instr isa chunk)
  "The comment contains information about the instruction but also about the operands.
   For example, if the instruction is a `load' instruction, the comment will contain the constant that is being loaded."
  (let* ((operand-values (instruction-operands instr))
         (isa-instr (instruction-by-opcode (instruction-opcode instr) isa))
         (isa-operands (isa-instruction-operands isa-instr))
         (operand-comments (loop :for isa-op :across isa-operands
                                 :for op :across operand-values
                                 :collect (comment-for op (isa-operand-type isa-op) chunk))))
    (format nil "~{~a~^, ~}" (remove-if #'null operand-comments))))

(defun comment-for (value op-type chunk)
  (case op-type
    (register nil)
    (address
     (let* ((constants (chunk-constants chunk))
            (constant (aref constants (the address-t value))))
       (format nil "~a = ~a" (format-operand value op-type) constant)))
    (t (unreachable! "Unknown operand type"))))

