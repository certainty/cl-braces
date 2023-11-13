(in-package :cl-user)

(defpackage :cl-braces.vm.runtime.value
  (:nicknames :vm.runtime.value :vm.value :value)
  (:use :cl)
  (:import-from :serapeum :->)
  (:export
   :value))

(defpackage :cl-braces.vm.bytecode
  (:nicknames :vm.bytecode :bytecode)
  (:use :cl :development)
  (:import-from :serapeum :->)
  (:export
   :chunk
   :chunk-code
   :chunk-constants
   :chunk-registers-used
   :constant-table

   :make-constants-builder
   :constants-add
   :make-chunk-builder
   :instruction-opcode
   :instruction-operands

   :add-constant
   :add-instructions

   :make-chunk
   :print-isa
   :*isa-1.0*
   :*current-isa*
   :with-opcodes-from-current-isa
   :operand-value
   :address-value
   :register-value
   :disass
   :disass-instruction

   :address-t
   :register-t
   :opcode-t

   :instr
   :address
   :addr
   :register
   :reg

   :loada
   :noop
   :halt
   :add
   :sub
   :div
   :mul
   :neg))

(defpackage :cl-braces.vm.machine
  (:nicknames :vm.machine :machine)
  (:use :cl :cl-braces.development)
  (:import-from :serapeum :->)
  (:import-from :cl-braces.vm.bytecode)
  (:export
   :execute))
