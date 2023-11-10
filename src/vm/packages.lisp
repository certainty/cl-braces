(in-package :cl-user)

(defpackage :cl-braces.vm.runtime.value
  (:nicknames :vm.runtime.value :vm.value :value)
  (:use :cl)
  (:import-from :serapeum :->)
  (:export
   :value))

(defpackage :cl-braces.vm.bytecode
  (:nicknames :vm.bytecode :bytecode)
  (:use :cl)
  (:import-from :serapeum :->)
  (:export
   :chunk
   :constant-table
   :make-chunk
   :print-isa
   :*isa-1.0*
   :operand-value
   :address-value
   :register-value

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
   :neg
   ))
