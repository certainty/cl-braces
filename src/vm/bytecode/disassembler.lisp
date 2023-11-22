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
    (a:when-let ((label (column-label pc instr isa chunk)))
      (format stream "~%~a:~%" label))
    (format stream "%~10a " (column-pc pc))
    (disass-instruction instr chunk :isa isa :stream stream)))

(defun disass-instruction (instr chunk &key (isa *current-isa*) (stream *standard-output*))
  (format stream "~16,a ~8,a ~30,a ~a~%"
          (column-encoded-instruction instr isa)
          (column-opcode instr isa)
          (column-operands instr isa)
          (column-comment instr isa chunk)))

(defmethod development:debug-print ((obj chunk))
  (disass obj :stream *debug-io*))

(defun column-pc (pc)
  (format nil "0x~X" pc))

(defun column-label (pc instr isa ch)
  "Prints the label for the given instruction."
  (declare (ignore instr isa))
  (let ((blocklabels (chunk-blocklabels ch)))
    (gethash pc blocklabels)))

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
    (format nil "~{~8,A~^ ~}" (loop :for isa-op :across isa-operands
                                    :for op :across operand-values
                                    :collect (format-operand op (isa-operand-type isa-op))))))

(defun format-operand (value op-type)
  "Formats the given operand value according to the given operand type.
   Registers are formatted as $<reg-number>, addresses as @<address-number>."
  (cond
    ((eq op-type 'register) (format nil "$~a" value))
    ((eq op-type 'address) (format nil "@~a" value))
    ((eq op-type 'label)   (format nil "%0x~X" value))
    (t (unreachable! "Unknown operand type"))))

(defun column-comment (instr isa chunk)
  "The comment contains information about the instruction but also about the operands.
   For example, if the instruction is a `load' instruction, the comment will contain the constant that is being loaded."
  (let* ((operand-values (instruction-operands instr))
         (isa-instr (instruction-by-opcode (instruction-opcode instr) isa))
         (isa-operands (isa-instruction-operands isa-instr))
         (operand-comments (remove-if #'null (loop :for isa-op :across isa-operands
                                                   :for op :across operand-values
                                                   :collect (comment-for op (isa-operand-type isa-op) chunk)))))
    (if (consp operand-comments)
        (format nil "// ~{~a~^, ~}" operand-comments)
        "")))

(defun comment-for (value op-type chunk)
  (let ((constants (chunk-constants chunk))
        (blocklabels (chunk-blocklabels chunk)))
    (case op-type
      (register nil)
      (label
       (a:when-let ((blocklabel (gethash value blocklabels)))
         (format nil "~a = ~a" (format-operand value op-type) blocklabel)))
      (address
       (let ((constant (aref constants value)))
         (format nil "~a = ~a" (format-operand value op-type) (format-constant constant))))
      (t (unreachable! "Unknown operand type")))))

(defun format-constant (constant)
  (trivia:match constant
    ((value:nilv) "nil")
    ((value:boolv b) (if b (format nil "true") (format nil "false")))
    ((value:intv n) (format nil "i~A" n))))
