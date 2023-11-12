(in-package :cl-braces.vm.machine)

(defparameter *debug-execution* nil)

(defun execute (chunk)
  (let ((pc (the fixnum -1))
        (opcode (the fixnum -1))
        (op1 (the fixnum -1))
        (op2 (the fixnum -1))
        (op3 (the fixnum -1)))
    (let ((instructions (bytecode:chunk-code chunk)))

      )))
