(in-package :cl-user)

(defpackage :cl-braces.isa
  (:nicknames :isa)
  (:use :cl)
  (:import-from
   :serapeum
   :->)
  (:export
   ;; instructions
   :+opcode-nop+
   :+opcode-halt+
   :+opcode-brk+
   :+opcode-call+
   :+opcode-ret+
   :+opcode-mov+
   :+opcode-loadk+
   :+opcode-loadi+
   :opcode->mnemonic

   :opcode-case

   :instruction
   :instr
   :instr-opcode
   :instr-op1
   :instr-op2
   :instr-op3

   :nop
   :ret
   :brk
   :mov
   :halt
   :loadi
   :loadk

   ;; chunk
   :chunk
   :chunk-constants
   :chunk-instructions))
