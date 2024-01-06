(in-package :cl-braces.vm.machine)

(defun dump-machine-state (headline machine pc registers &key (disass-chunk t) (dump-instruction nil) (dump-stack nil))
  (with-slots (chunk call-stack) machine
    (format t "### ~A ###~%~%" headline)
    (format t "PC:           ~3,'0X~%" pc)
    (format t "Registers:    ~{~a~^, ~} ~%" (loop for reg across registers for i from 0 collect (format nil "R~A=~A" i (format-register reg chunk))))

    (when dump-instruction
      (format t "Instruction:  ")
      (bytecode::disass-instruction (aref (bytecode:chunk-code chunk) (1- pc)) chunk))

    (when dump-stack
      (with-slots (frames stack-pointer) call-stack
        (format t "~%#### Call-Stack #### ~%~%")
        (format t "~10,a ~20,a ~5,a ~%" "Pos" "Label" "Return Address")

        (loop :for i :from (1- stack-pointer) :downto 0
              :do (format-frame i (aref frames i) chunk))))

    (when disass-chunk
      (format t "~%#### Disassembly #### ~%")
      (bytecode:disass chunk))
    (format t "~%")))

(defun format-register (reg chunk)
  (bytecode:format-value reg chunk))

(defun format-frame (stack-pos frame chunk)
  (let ((function-label (runtime.value:closure-function-label (call-frame-function frame))))
    (format t "~10,a ~20,a %~3,'0X ~%" stack-pos (bytecode:label-name-for-label chunk function-label) (call-frame-return-address frame))))
