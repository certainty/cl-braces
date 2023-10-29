(in-package :cl-braces.vm)

;; (defun disass (chunk)
;;   (loop
;;     for addr from 0
;;     for instruction across (bytecode:chunk-instructions chunk)
;;     do (disass-instruction instruction addr)))

;; (defun disass-instruction (instruction addr &optional (stream *standard-output*))
;;   (format stream "~4,'0d: 0x~X ~6a ~@[~a~] ~@[~a~] ~@[~a~] ~%"
;;           addr
;;           (bytecode:instr-opcode instruction)
;;           "don' know"
;;                                         ;(bytecode:opcode->mnemonic (isa:instr-opcode instruction))
;;           (bytecode:instr-op1 instruction)
;;           (bytecode:instr-op2 instruction)
;;           (bytecode:instr-op3 instruction)))

;; (defun disass-operand (operand)
;;   ;; keep it simple for now
;;   (typecase operand
;;     (bytecode:operand-register (format nil "R~d" (bytecode:reg-value operand)))
;;     (bytecode:operand-immediate (format nil "I~d" (bytecode:immediate-value operand)))))
