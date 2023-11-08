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
   :make-chunk
   :print-isa
   :*isa-1.0*

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
