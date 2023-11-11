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
   :constant-table
   :make-constants-builder
   :constants-add
   :make-chunk-builder

   :add-constant
   :add-instructions

   :make-chunk
   :print-isa
   :*isa-1.0*
   :*current-isa*
   :operand-value
   :address-value
   :register-value
   :disass

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
  (:use :cl)
  (:import-from :cl-braces.vm.bytecode)
  (:export
   :execute))
