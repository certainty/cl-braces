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

(defmacro binary-op (operation instruction registers)
  `(with-operands (dst lhs rhs) instruction
     (setf (aref ,registers dst)
           (runtime.value:box (,operation (runtime.value:unbox (aref ,registers lhs))
                                          (runtime.value:unbox (aref ,registers rhs)))))))

(defmacro unary-op (operation instruction registers)
  `(with-operands (dst src) instruction
     (setf (aref ,registers dst) (runtime.value:box (,operation (runtime.value:unbox (aref ,registers src)))))))

(defun run (input &key (fail-fast nil))
  (let ((chunk (compiler:compile-this input :fail-fast fail-fast)))
    (execute chunk)))

(defun run-source-file (input &key (fail-fast) (entry-point nil))
  (let ((chunk (compiler:compile-source-file input :fail-fast fail-fast)))
    (when entry-point
      (setf (slot-value chunk 'bytecode::entrypoint) entry-point))
    (execute chunk)))

(defclass call-frame ()
  ((registers
    :initarg :registers
    :initform (vector))
   (return-address
    :initarg :return-address
    :initform (error "return-address not specified"))))

(defun make-call-frame (number-of-registers return-address)
  (make-instance 'call-frame :registers (make-registers number-of-registers) :return-address return-address))

(defun make-registers (amount)
  (make-array amount :element-type '(or null runtime.value:<value> bytecode:register-t) :initial-element nil))

(defun execute (chunk)
  (unless (bytecode:chunk-entrypoint chunk)
    (error "Chunk has no entrypoint"))

  (let* ((pc (the fixnum (bytecode:chunk-entrypoint chunk)))
         (instructions (bytecode:chunk-code chunk))
         (instruction-count (length instructions))
         (instruction (the fixnum 0))
         (zero-flag nil)
         (opcode (the fixnum 0))
         (call-stack nil)
         (constants (bytecode:chunk-constants chunk))
         (registers (make-registers 50)))
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

          (bytecode:const
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

          (bytecode:add (binary-op + instruction registers))
          (bytecode:sub (binary-op - instruction registers))
          (bytecode:mul (binary-op * instruction registers))
          (bytecode:div (binary-op / instruction registers))
          (bytecode:neg (unary-op  - instruction registers))
          (bytecode:eq  (binary-op = instruction registers))

          (t (todo! "unsupported opcode")))))

    #-cl-braces-vm-release
    (dump-machine-state "Final machine state" pc chunk registers :disass-chunk nil :dump-instruction nil)
    (values (find-if-not #'null registers :from-end t) registers)))
