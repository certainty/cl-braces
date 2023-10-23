(in-package :cl-braces/vm)

(defmacro define-opcodes (&rest opcodes)
  (let ((counter 0)
        (constants (list))
        (type-body (list)))
    (dolist (entry opcodes)
      (let ((name (first entry))
            (docstring (second entry)))
        (push `(defconstant ,name ,counter ,docstring) constants)
        (push counter type-body)
        (incf counter)))
    `(progn
       ,@(eval-when (:compile-toplevel :load-toplevel :execute)
           (reverse constants))
       (deftype tpe-opcode ()
         '(member ,@(reverse type-body))))))

(define-opcodes
    (+opcode-nop+ "nop - no operation")
    (+opcode-halt+ "halt - halt the virtual machine")
  (+opcode-call+ "call r0 r1 r2 - call a function, r0 is the function, r1 is the number of arguments, r2 is the number of return values")
  (+opcode-ret+ "ret - return from a function")
  (+opcode-move+ "mov dst src - move between registers")
  (+opcode-loadk+ "loadk dst k - load a constant into a register "))

(defstruct operand-register
  (value 0 :type integer :read-only t))

(defstruct operand-immediate
  (value 0 :read-only t))

(deftype tpe-operand ()
  '(or operand-register operand-immediate))

(defstruct (instruction (:conc-name instr-))
  (opcode (error "must supply opocode") :type tpe-opcode :read-only t)
  (operands (error "must supply operands") :type (vector tpe-operand *) :read-only t))

(defstruct (call-frame (:constructor make-call-frame (&key (allocate-registers 15))))
  (registers (make-array allocate-registers :initial-element 0)))

(defvar *stack-size* 1024)

(defstruct (virtual-machine (:conc-name vm-) (:constructor make-vm (&key (stack-size *stack-size*) (allocate-registers 16))))
  (registers (make-array allocate-registers :initial-element 0))
  (call-stack (make-array stack-size :initial-element 0 :element-type 'call-frame))
  (instruction-pointer 0))

(defvar *current-chunk* nil "The currently executed chunk")

(-> run (virual-machine chunk) t)
(defun run (vm chunk &key (entry-point 0))
  (setf (vm-instruction-pointer vm) entry-point)
  (let ((*current-chunk* chunk))
    (loop
      (let ((instruction (aref *current-chunk* (vm-instruction-pointer vm))))
        (incf (vm-instruction-pointer vm))
        (ecase-of tpe-opcode (instr-opcode instruction)
          (+opcode-nop+ (continue))
          (+opcode-halt+ (return-from run))
          (+opcode-call+ (continue))
          (+opcode-ret+ (continue))
          (+opcode-move+ (continue))
          (+opcode-loadk+ (continue)))))))
