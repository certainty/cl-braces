(in-package :cl-braces/vm)

(defun disass (chunk)
  (loop
    for addr from 0
    for instruction across (chunk-instructions chunk)
    do (disass-instruction instruction addr)))

(defun disass-instruction (instruction addr &optional (stream *standard-output*))
  (format stream "~4,'0d: 0x~X ~6a ~@[~a~] ~@[~a~] ~@[~a~] ~%" addr (instr-opcode instruction) (aref *mnemonics* (instr-opcode instruction)) (instr-op1 instruction) (instr-op2 instruction) (instr-op3 instruction)))

(defun disass-operand (operand)
  ;; keep it simple for now
  (typecase operand
    (operand-register (format nil "R~d" (reg-value operand)))
    (operand-immediate (format nil "I~d" (immediate-value operand)))))
