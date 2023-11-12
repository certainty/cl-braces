(in-package :cl-braces.vm.machine)

(defparameter *debug-execution* nil)

(defun execute (chunk)
  (let* ((pc (the fixnum 0))
         (instructions (bytecode:chunk-code chunk))
         (instruction-count (length instructions))
         (instruction nil)
         (opcode (the fixnum 0)))

    (when *debug-execution*
      (bytecode:disass chunk))

    (bytecode:with-opcodes-from-isa
      (loop
        (when (>= pc instruction-count)
          (return))

        (setf instruction (aref instructions pc))
        (setf opcode (bytecode:instruction-opcode instruction))
        (incf pc)

        (when *debug-execution*
          (bytecode:disass-instruction chunk pc instruction))

        (case opcode
          (bytecode:noop t)
          (bytecode:halt (return))
          (bytecode:sub (todo! "sub not implemented"))
          (bytecode:loada (todo! "loada not implemented"))
          (otherwise (todo! "unsupported opcode")))))))
