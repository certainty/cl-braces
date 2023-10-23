(in-package :cl-braces/vm)

(defparameter *vm* nil "The virtual machine executing")
(defparameter *stack-size* 1024 "The stacksize of the machine")
(defparameter *current-chunk* nil "The currently executed chunk")
(defparameter *current-stack-frame* nil "The currently executed stackframe")

(defmacro define-opcodes (&rest opcodes)
  (let ((counter 0)
        (constants (list))
        (mnemonics (list))
        (type-body (list)))
    (dolist (entry opcodes)
      (let ((name (first entry))
            (mnemonic (second entry))
            (docstring (third entry)))
        (push `(defconstant ,name ,counter ,docstring) constants)
        (push mnemonic mnemonics)
        (push counter type-body)
        (incf counter)))
    `(progn
       ,@(eval-when (:compile-toplevel :load-toplevel :execute)
           (reverse constants))
       (defparameter *mnemonics* ,(coerce (reverse mnemonics) 'vector))
       (deftype tpe-opcode ()
         '(member ,@(reverse type-body))))))

(define-opcodes
    (+opcode-nop+ "NOP" "no operation")
    (+opcode-halt+ "HALT" "halt the virtual machine")
  (+opcode-call+ "CALL" "call r0 r1 r2 - call a function, r0 is the function, r1 is the number of arguments, r2 is the number of return values")
  (+opcode-ret+ "RED" "ret - return from a function")
  (+opcode-move+ "MOV" "mov dst src - move between registers")
  (+opcode-loadk+ "LOADK" "loadk dst k - load a constant into a register "))

(defstruct (operand-register (:conc-name reg-) (:constructor reg (provided-value)))
  (value provided-value :type integer :read-only t))

(defstruct (operand-immediate (:conc-name immediate-) (:constructor imm (provided-value)))
  (value provided-value :read-only t))

(deftype tpe-operand ()
  '(or operand-register operand-immediate))

(defstruct (instruction (:conc-name instr-) (:constructor instr (provided-opcode &rest provided-operands)))
  (opcode provided-opcode :type tpe-opcode :read-only t)
  (operands (coerce provided-operands 'vector) :type (vector tpe-operand *) :read-only t))

(defstruct (chunk (:conc-name chunk-) (:constructor chunk (&rest provided-instructions)))
  (instructions (make-array (length provided-instructions) :element-type 'instruction :initial-contents provided-instructions) :type (vector instruction *) :read-only t))

(defstruct (call-frame (:constructor make-call-frame (&key (allocate-registers 15))))
  (registers (make-array allocate-registers :initial-element 0)))

(defstruct (virtual-machine (:conc-name vm-) (:constructor make-vm (&key (stack-size *stack-size*) (allocate-registers 16))))
  (registers (make-array allocate-registers :initial-element 0))
  (call-stack (make-array stack-size :initial-element 0 :element-type 'call-frame))
  (instruction-pointer 0))

;; TODO: add compile time exhaustiveness check
(defmacro opcode-case (opcode &rest executions)
  `(let ((next-opcode ,opcode))
     (cond
       ,@(loop for exec in executions collect `((= next-opcode ,(first exec)) ,@(cdr exec)))
       (t (error "VM bug")))))

(defvar *test-chunk*
  (chunk
   (instr +opcode-loadk+ (reg 0) (imm 1))
   (instr +opcode-halt+)))

(-> disass (chunk) string)
(defun disass (chunk)
  (loop
    for addr from 0
    for instruction across (chunk-instructions chunk)
    do (disass-instruction instruction addr)))

(defun disass-instruction (instruction addr &optional (stream *standard-output*))
  (let ((operands (loop for operand across (instr-operands instruction) collect (disass-operand operand))))
    (format stream "~4,'0d: 0x~X ~6a ~{~a~^ ~}~%" addr (instr-opcode instruction) (aref *mnemonics* (instr-opcode instruction)) operands)))

(defun disass-operand (operand)
  ;; keep it simple for now
  (typecase operand
    (operand-register (format nil "r~d" (reg-value operand)))
    (operand-immediate (format nil "~d" (immediate-value operand)))))

(defun run-chunk (vm chunk &key (entry-point 0))
  (let ((*vm* vm)
        (*current-chunk* chunk))
    (setf (vm-instruction-pointer vm) entry-point)
    (loop
      (let ((instruction (aref chunk (vm-instruction-pointer vm))))
        (incf (vm-instruction-pointer vm))
        (opcode-case (instr-opcode instruction)
                     (+opcode-nop+ (continue))
                     (+opcode-halt+ (return-from run-chunk))
                     (+opcode-call+ (continue))
                     (+opcode-ret+ (continue))
                     (+opcode-move+ (continue))
                     (+opcode-loadk+ (continue)))))))
