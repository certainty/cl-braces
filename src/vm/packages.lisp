(in-package :cl-user)

(defpackage :cl-braces.vm
  (:nicknames :vm)
  (:use :cl)
  (:import-from :serapeum
   :->))

(defpackage :cl-braces.vm.runtime
  (:nicknames :vm.runtime :runtime)
  (:use :cl)
  (:import-from :serapeum
   :->)
  (:export
   :value))

(defpackage :cl-braces.vm.bytecode
  (:nicknames :vm.bytecode :bytecode)
  (:use :cl)
  (:import-from :serapeum
   :->)
  (:export
   :chunk
   :instruction
   :instr-opcode
   :instr-op1
   :instr-op2
   :instr-op3

   :+opcode-nop+
   :+opcode-halt+
   :+opcode-ret+
   :+opcode-brk+
   :+opcode-loadi+
   :+opcode-loadk+
   :+opcode-mov+
   :+opcode-call+
   :opcode-case

   :loadi
   :loadk
   :mov
   :ret
   :brk
   :nop
   :halt))
