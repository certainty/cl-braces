(in-package :cl-braces.vm.machine)

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
  (format t "Registers:    ~{~a~^, ~} ~%" (loop for reg across registers for i from 0 collect (format nil "R~A=~A" i (format-register reg))))

  (when dump-instruction
    (format t "Instruction: ")
    (bytecode::disass-instruction (aref (bytecode:chunk-code chunk) (1- pc)) chunk))

  (when disass-chunk
    (format t "Disassembly: ~%")
    (bytecode:disass chunk))

  (format t "~%"))

(defun format-register (reg)
  (trivia:match reg
    ((runtime.value:nilv) "nil")
    ((runtime.value:boolv b) (if b "true" "false"))
    ((runtime.value:intv n) (format nil "i~A" n))
    (_ (format nil "R~A" reg))))

(defmacro binary-op (operation instruction registers result-register)
  `(with-operands (dst lhs rhs) instruction
     (setf ,result-register dst)
     (setf (aref ,registers dst)
           (runtime.value:box (,operation (runtime.value:unbox (aref ,registers lhs))
                                          (runtime.value:unbox (aref ,registers rhs)))))))

(defmacro unary-op (operation instruction registers result-register)
  `(with-operands (dst) instruction
     (setf ,result-register dst)
     (setf (aref ,registers dst) (runtime.value:box (,operation (runtime.value:unbox (aref ,registers dst)))))))

(-> execute (bytecode:chunk) (values runtime.value:value &optional))
(defun execute (chunk)
  (let* ((pc (the fixnum 0))
         (instructions (bytecode:chunk-code chunk))
         (instruction-count (length instructions))
         (instruction nil)
         (result-reg nil)
         (zero-flag nil)
         (constants (bytecode:chunk-constants chunk))
         (registers (make-array (bytecode:chunk-registers-used chunk) :element-type '(or runtime.value:value bytecode:register-t) :initial-element runtime.value:nilv)))


    #-cl-braces-vm-release
    (progn
      (format t "## Execute ~%~%")
      (dump-machine-state "Initial machine state" pc chunk registers :disass-chunk t))

    (bytecode:with-opcodes-from-current-isa
      (loop
        (when (>= pc instruction-count)
          (return))

        (setf instruction (aref instructions pc))
        (setf opcode (bytecode:instruction-opcode instruction))
        (incf pc)

        #-cl-braces-vm-release
        (dump-machine-state "Execute instruction" pc chunk registers :disass-chunk nil :dump-instruction t)

        (s:select opcode
          (bytecode:noop t)
          (bytecode:halt (return))

          (bytecode:loada
           (with-operands (dst addr) instruction
             (setf result-reg dst)
             (setf (aref registers dst) (aref constants addr))))

          (bytecode:mov
           (with-operands (dst src) instruction
             (setf result-reg dst)
             (setf (aref registers dst) (aref registers src))))

          (bytecode:test
           (with-operands (dst) instruction
             (setf zero-flag (runtime.value:falsep (aref registers dst)))))

          (bytecode:jmp
           (with-operands (addr) instruction
             (setf pc addr)))

          (bytecode:jz
           (with-operands (addr) instruction
             (when zero-flag
               (setf pc addr))))

          (bytecode:jnz
           (with-operands (addr) instruction
             (unless zero-flag
               (setf pc addr))))

          (bytecode:add (binary-op + instruction registers result-reg))
          (bytecode:sub (binary-op - instruction registers result-reg))
          (bytecode:mul (binary-op * instruction registers result-reg))
          (bytecode:div (binary-op / instruction registers result-reg))
          (bytecode:neg (unary-op - instruction registers result-reg))
          (t (todo! "unsupported opcode")))))

    #-cl-braces-vm-release
    (dump-machine-state "Final machine state" pc chunk registers :disass-chunk nil :dump-instruction nil)

    (values (aref registers result-reg))))
