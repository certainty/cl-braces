(in-package :cl-braces.vm.bytecode)

(defun disass (chunk &key (isa *current-isa*) (stream *standard-output*))
  "Disassemble a chunk of bytecode. It uses the following format:
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
  (declare (ignore isa))
  (with-output-to-string (stream)
    (let ((opcode (instruction-opcode instr))
          (operands (instruction-operands instr)))
      (format stream "~2,'0X" opcode)
      (loop for operand across operands
            do (format stream "~2,'0X" operand)))))

(defun column-opcode (instr isa)
  (let* ((opcode (instruction-opcode instr))
         (isa-instr (instruction-by-opcode opcode isa)))
    (assert isa-instr)
    (format nil "~a" (isa-instruction-mnemonic isa-instr))))

(defun column-operands (instr isa)
  (let* ((operand-values (instruction-operands instr))
         (isa-instr (instruction-by-opcode (instruction-opcode instr) isa))
         (isa-operands (isa-instruction-operands isa-instr)))
    (format nil "~{~a~^, ~}" (loop :for isa-op :across isa-operands
                                   :for op :across operand-values
                                   :collect (format-operand op (isa-operand-type isa-op))))))

(defun format-operand (value op-type)
  (cond
    ((eq op-type 'register) (format nil "$~a" value))
    ((eq op-type 'address) (format nil "@~a" value))
    (t (unreachable! "Unknown operand type"))))

(defun column-comment (instr isa chunk)
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

