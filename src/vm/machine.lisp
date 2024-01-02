(in-package :cl-braces.vm.machine)

(defparameter *MACHINE* nil)
(defparameter *STACK-SIZE* 10)

(define-condition machine-error (error)
  ((message
    :initarg :message
    :initform nil)))

(define-condition runtime-error (machine-error) ())

(defclass machine ()
  ((call-stack
    :initarg :call-stack
    :initform nil)
   (chunk
    :initform nil
    :type bytecode:chunk)))

(defun make-machine (&key (stack-size *STACK-SIZE*))
  (make-instance 'machine :call-stack (make-call-stack stack-size)))

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
  (let* ((machine (make-machine))
         (chunk (compiler:compile-string input :fail-fast fail-fast))
         (*MACHINE* machine))
    (load-code machine chunk)
    (execute machine)))

(defun run-source-file (input &key (fail-fast) (entry-point nil))
  (let* ((machine (make-machine))
         (chunk (compiler:compile-source-file input :fail-fast fail-fast))
         (*MACHINE* machine))
    (load-code machine chunk)
    (execute machine)))

(defun load-code (machine provided-chunk)
  (prog1 machine
    (with-slots (call-stack chunk pc) machine
      (call-stack-reset call-stack)
      (setf chunk provided-chunk))))

(defun execute (machine)
  (with-slots (chunk call-stack) machine
    (let* ((entrypoint (bytecode:chunk-entrypoint chunk))
           (constants  (bytecode:chunk-constants chunk))
           (instructions (bytecode:chunk-code chunk))
           (instruction-count (length instructions)))

      (unless entrypoint
        (error "Chunk has no entrypoint"))

      (let* ((pc          (the fixnum (bytecode:label-address entrypoint)))
             (instruction (the fixnum 0))
             (opcode (the fixnum 0))
             (zero-flag nil)
             (initial-registers (make-registers (bytecode:chunk-registers-used chunk)))
             (registers initial-registers))

        #-cl-braces-vm-release
        (progn
          (format t "## Execute ~%~%")
          (dump-machine-state "Initial machine state" machine pc registers :disass-chunk t)
          (terpri))

        (bytecode:with-opcodes-from-current-isa

          (loop
            (when (>= pc instruction-count)
              (return))

            (setf instruction (aref instructions pc))
            (setf opcode (bytecode:instruction-opcode instruction))
            (incf pc)

            #-cl-braces-vm-release
            (dump-machine-state "Execute instruction" machine pc registers :disass-chunk nil :dump-instruction t :dump-stack t)

            (s:select opcode
              (bytecode:noop t)
              (bytecode:halt (return))

              (bytecode:const
               (with-operands (dst addr) instruction
                 (setf (aref registers dst) (aref constants addr))))

              (bytecode:mov
               (with-operands (dst src) instruction
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

              (bytecode:call
               (with-operands (fn argc) instruction
                 (let ((closure (aref registers fn)))
                   (unless (runtime.value:closurep closure)
                     (error 'runtime-error :message "Attempt to call non-function value ~S" fn))
                   (multiple-value-bind (address call-frame) (prepare-function-call pc closure argc registers)
                     (call-stack-push call-stack call-frame)
                     (setf registers (slot-value call-frame 'registers))
                     (setf pc address)))))

              (bytecode:ret
               (let ((call-frame (call-stack-pop call-stack)))
                 (unless call-frame
                   (error 'runtime-error :message "Attempt to return from empty call stack"))
                 ;; restore the registers
                 (if (call-stack-empty-p call-stack)
                     (setf registers initial-registers)
                     (setf registers (call-stack-registers (call-stack-top call-frame))))
                 (setf pc (call-frame-return-address call-frame))))

              (t (todo! "unsupported opcode")))))

        #-cl-braces-vm-release
        (dump-machine-state "Final machine state" machine pc registers :disass-chunk nil :dump-instruction nil)
        (values (find-if-not #'null registers :from-end t) registers)))))

(defun prepare-function-call (return-addr closure argc source-registers)
  (let ((call-frame (make-call-frame closure return-addr))
        (arity (runtime.value:closure-arity closure))
        (args-begin (- (length source-registers) 1 argc)))

    (unless (eq (runtime.value:arity-kind arity) :fixed)
      (todo! "handle non-fixed arity"))

    ;; transfer arguments in the first registers
    (with-slots (registers) call-frame
      (loop :for i :from args-begin :below (length source-registers)
            :for j :from 0
            :do (setf (aref registers j) (aref source-registers i))))

    (values
     (bytecode:label-address (runtime.value:closure-function-label closure))
     call-frame)))
