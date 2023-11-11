(in-package :cl-braces.vm.bytecode)

;;;; The default instruction set architecture (ISA) for the bytecode VM
;;;; Overall design decisions:
;;;;
;;;; 1. all instructions follow intel style operator ordering (i.e. dst, lhs, rhs)
;;;; 2. opcodes indicate their arity via the two least significant bits
;;;;   .a nullary opcodes have their LSPs set to 00
;;;;   .b unary opcodes have their LSPs set to 01
;;;;   .c binary opcodes have their LSPs set to 10
;;;;   .d ternary opcodes have their LSPs set to 11
;;;; 3. when an operation exists for different kinds of operand types, we usually opt for a separate opcode.
;;;;    for example if we load a constant from an address or an immediate value, we have two separate opcodes.
;;;;

(define-isa *isa-1.0*
  :version (1 0)
  :instructions
  (
   ;; nullary instructions #x00 - #x3f
   (#x00 halt  ()                              "Halts the execution of the vm")
   (#x01 noop  ()                              "NOOP")
   (#x02 brk   ()                              "Interrupt the flow of the program an invoke the debugger.")

   ;; unary instructions #x40 - #x7f
   (#x40 neg   ((reg dst))                     "Negatate value in register $dst. The result is written directly to $dst.")

   ;; binary instructions #x80 - #xbf


   ;; ternary instructions #xc0 - #xff
   (#xC0 sub   ((reg dst) (reg lhs) (reg rhs)) "Subtracts what is in $lhs from $rhs and stores the result in $dst.")
   (#xC1 add   ((reg dst) (reg lhs) (reg rhs)) "Add what is in $lhs to $rhs and stores the result in $dst.")
   (#xC2 mul   ((reg dst) (reg lhs) (reg rhs)) "Multiplies what is in $lhs by $rhs and stores the result in $dst.")
   (#xC3 div   ((reg dst) (reg lhs) (reg rhs)) "Divides what is in $lhs by $rhs and stores the result in $dst.")
   (#xC4 loada ((reg dst) (addr src))          "Loads the constant from $src into $dst")))

(setf *current-isa* *isa-1.0*)
