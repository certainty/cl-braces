(in-package :cl-braces.vm.machine)

(defparameter *debug-execution* nil)

(-> execute (bytecode:chunk) (values value:value &optional))
(defun execute (chunk)
  (let* ((pc (the fixnum 0))
         (instructions (bytecode:chunk-code chunk))
         (instruction-count (length instructions))
         (instruction nil)
         (result-reg nil)
         (opcode (the fixnum 0))
         (constants (bytecode:chunk-constants chunk))
         (registers (make-array (bytecode:chunk-registers-used chunk) :element-type 'value:value :initial-element 0)))

    (when *debug-execution*
      (format t "Executing next chunk: ~%")
      (bytecode:disass chunk))

    (bytecode:with-opcodes-from-current-isa
      (loop
        (when (>= pc instruction-count)
          (return))

        (setf instruction (aref instructions pc))
        (setf opcode (bytecode:instruction-opcode instruction))
        (incf pc)

        (when *debug-execution*
          (format t "~%[~3,'0X] Next instruction~%" pc)
          (format t "=============================~%")
          (format t "~A~%" registers)
          (format t "=============================~%")
          (bytecode:disass-instruction pc instruction chunk))

        (cond
          ((= bytecode:noop opcode) t)
          ((= bytecode:halt opcode) (return))
          ((= bytecode:loada opcode)
           (let ((operands (bytecode:instruction-operands instruction)))
             (let ((dst (aref operands 0))
                   (addr (aref operands 1)))
               (setf result-reg dst)
               (setf (aref registers dst) (aref constants addr)))))
          ((= bytecode:add opcode)
           (let ((operands (bytecode:instruction-operands instruction)))
             (let ((dst (aref operands 0))
                   (lhs (aref operands 1))
                   (rhs (aref operands 2)))
               (setf result-reg dst)
               (setf (aref registers dst) (+ (aref registers lhs) (aref registers rhs))))))

          ((= bytecode:sub opcode)
           (let ((operands (bytecode:instruction-operands instruction)))
             (let ((dst (aref operands 0))
                   (lhs (aref operands 1))
                   (rhs (aref operands 2)))
               (setf result-reg dst)
               (setf (aref registers dst) (- (aref registers lhs) (aref registers rhs))))))

          ((= bytecode:mul opcode)
           (let ((operands (bytecode:instruction-operands instruction)))
             (let ((dst (aref operands 0))
                   (lhs (aref operands 1))
                   (rhs (aref operands 2)))
               (setf result-reg dst)
               (setf (aref registers dst) (* (aref registers lhs) (aref registers rhs))))))

          ((= bytecode:div opcode)
           (let ((operands (bytecode:instruction-operands instruction)))
             (let ((dst (aref operands 0))
                   (lhs (aref operands 1))
                   (rhs (aref operands 2)))
               (setf result-reg dst)
               (setf (aref registers dst) (/ (aref registers lhs) (aref registers rhs))))))

          ((= bytecode:neg opcode)
           (let ((operands (bytecode:instruction-operands instruction)))
             (let ((dst (aref operands 0)))
               (setf result-reg dst)
               (setf (aref registers dst) (- (aref registers dst))))))

          (t (todo! "unsupported opcode")))))

    ;; dump the machine state
    (when *debug-execution*
      (format t "~%=============================~%Execution finished.~%")
      (format t "PC: ~3,'0X~%" pc)
      (format t "Registers: ~A~%" registers)
      (format t "Result: ~A~%" (aref registers result-reg)))
    (values (aref registers result-reg))))
