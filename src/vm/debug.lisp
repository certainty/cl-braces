(in-package :cl-braces.vm.machine)

(defun dump-machine-state (headline pc chunk registers &key (disass-chunk t) (dump-instruction nil))
  (format t "### ~A ###~%~%" headline)
  (format t "PC:           ~3,'0X~%" pc)
  (format t "Registers:    ~{~a~^, ~} ~%" (loop for reg across registers for i from 0 collect (format nil "R~A=~A" i (format-register reg chunk))))

  (when dump-instruction
    (format t "Instruction:  ")
    (bytecode::disass-instruction (aref (bytecode:chunk-code chunk) (1- pc)) chunk))

  (when disass-chunk
    (format t "~%#### Disassembly #### ~%")
    (bytecode:disass chunk))
  (format t "~%"))

(defun format-register (reg chunk)
  (bytecode:format-value reg chunk))
