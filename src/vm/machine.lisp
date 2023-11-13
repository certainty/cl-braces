(in-package :cl-braces.vm.machine)

(defun debug-vm (&optional (debug t))
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

(defun dump-machine-state (headline pc chunk registers &key (disass-chunk t) (dump-instruction nil))
  (format t "### ~A ###~%~%" headline)
  (format t "PC:           ~3,'0X~%" pc)
  (format t "Registers:    ~{~a~^, ~} ~%" (loop for reg across registers for i from 0 collect (format nil "R~A=~A" i reg)))

  (when dump-instruction
    (format t "Instruction: ")
    (bytecode::disass-instruction (aref (bytecode:chunk-code chunk) (1- pc)) chunk))

  (when disass-chunk
    (format t "Disassembly: ~%")
    (bytecode:disass chunk))

  (format t "~%"))

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
    (dump-machine-state "Initial machine state" pc chunk registers :disass-chunk t)

    (bytecode:with-opcodes-from-current-isa
      (loop
        (when (>= pc instruction-count)
          (return))

        (setf instruction (aref instructions pc))
        (setf opcode (bytecode:instruction-opcode instruction))
        (incf pc)

        #+cl-braces-debug-vm
        (dump-machine-state "Execute instruction" pc chunk registers :disass-chunk nil :dump-instruction t)

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

    #+cl-braces-debug-vm
    (dump-machine-state "Final machine state" pc chunk registers :disass-chunk nil :dump-instruction nil)

    (values (aref registers result-reg))))
