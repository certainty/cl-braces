(in-package :cl-braces.vm)

(defun disass (chunk)
  (loop
    for addr from 0
    for instruction across (isa:chunk-instructions chunk)
    do (disass-instruction instruction addr)))

(defun disass-instruction (instruction addr &optional (stream *standard-output*))
  (format stream "~4,'0d: 0x~X ~6a ~@[~a~] ~@[~a~] ~@[~a~] ~%"
          addr
          (isa:instr-opcode instruction)
          (isa:opcode->mnemonic (isa:instr-opcode instruction))
          (isa:instr-op1 instruction)
          (isa:instr-op2 instruction)
          (isa:instr-op3 instruction)))

(defun disass-operand (operand)
  ;; keep it simple for now
  (typecase operand
    (isa:operand-register (format nil "R~d" (isa:reg-value operand)))
    (isa:operand-immediate (format nil "I~d" (isa:immediate-value operand)))))
