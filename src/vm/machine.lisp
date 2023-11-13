(in-package :cl-braces.vm.machine)

(defun cl-debug-vm (&optional (debug t))
  (if debug
      (push :cl-braces-debug-vm *features*)
      (setf *features* (remove :cl-braces-debug-vm *features*))))

(defmacro with-operands ((operand &rest operands) instruction &body body)
  (let ((operand-count (length (cons operand operands)))
        (operands-var (gensym))
        (all-operands (cons operand operands)))
    `(let ((,operands-var (bytecode:instruction-operands ,instruction)))
       (let ,(loop for i from 0 below operand-count
                   for operand in all-operands
                   collect `(,operand (aref ,operands-var ,i)))
         ,@body))))

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

    #+cl-braces-debug-vm
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

        #+cl-braces-debug-vm
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
           (with-operands (dst addr) instruction
             (setf result-reg dst)
             (setf (aref registers dst) (aref constants addr))))
          ((= bytecode:add opcode)
           (with-operands (dst lhs rhs) instruction
             (setf result-reg dst)
             (setf (aref registers dst) (+ (aref registers lhs) (aref registers rhs)))))

          ((= bytecode:sub opcode)
           (with-operands (dst lhs rhs) instruction
             (setf result-reg dst)
             (setf (aref registers dst) (- (aref registers lhs) (aref registers rhs)))))

          ((= bytecode:mul opcode)
           (with-operands (dst lhs rhs) instruction
             (setf result-reg dst)
             (setf (aref registers dst) (* (aref registers lhs) (aref registers rhs)))))

          ((= bytecode:div opcode)
           (with-operands (dst lhs rhs) instruction
             (setf result-reg dst)
             (setf (aref registers dst) (/ (aref registers lhs) (aref registers rhs)))))

          ((= bytecode:neg opcode)
           (with-operands (dst) instruction
             (setf result-reg dst)
             (setf (aref registers dst) (- (aref registers dst)))))
          (t (todo! "unsupported opcode")))))

    ;; dump the machine state
    #+cl-braces-debug-vm
    (when *debug-execution*
      (format t "~%=============================~%Execution finished.~%")
      (format t "PC: ~3,'0X~%" pc)
      (format t "Registers: ~A~%" registers)
      (format t "Result: ~A~%" (aref registers result-reg)))

    (values (aref registers result-reg))))
