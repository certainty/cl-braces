(defpackage :cl-braces/isa/instructions
  (:use #:cl #:serapeum)
  (:export
   #:+opcode-nop+
   #:+opcode-halt+
   #:+opcode-brk+
   #:+opcode-call+
   #:+opcode-ret+
   #:+opcode-mov+
   #:+opcode-loadk+
   #:+opcode-loadi+

   #:opcode-case

   #:instruction
   #:instr
   #:instr-opcode
   #:instr-op1
   #:instr-op2
   #:instr-op3

   #:nop
   #:ret
   #:brk
   #:mov
   #:halt
   #:loadi
   #:loadk))

(defpackage :cl-braces/isa/chunk
  (:use #:cl #:serapeum #:cl-braces/isa/instructions)
  (:export
   #:chunk
   #:chunk-constants
   #:chunk-instructions))

(defpackage :cl-braces/isa/value
  (:use #:cl #:serapeum))

(defpackage :cl-braces/isa
  (:use #:cl-braces/isa/instructions))
