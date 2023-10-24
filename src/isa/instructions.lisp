(in-package :cl-braces/isa/instructions)

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

(defmacro opcode-case (opcode &rest executions)
  (let ((next-opcode (gensym)))
    `(let ((,next-opcode ,opcode))
       (cond
         ,@(loop for exec in executions collect `((= ,next-opcode ,(first exec)) ,@(cdr exec)))
         (t (error "VM bug"))))))

(define-opcodes
    (+opcode-nop+ "NOP" "no operation")
    (+opcode-halt+ "HALT" "halt the virtual machine")
  (+opcode-brk+ "BRK" "break the execution at this point and start the interactive debugger")
  (+opcode-call+ "CALL" "r0 r1 r2 => call a function, `r0' is the function, `r1' is the number of arguments, `r2' is the number of return values. Arguments are expected to be placed in the first registers of the callframe.")
  (+opcode-ret+ "RET" "return from a function")
  (+opcode-mov+ "MOV" "dst src => move the value from `src' to `dst'")
  (+opcode-loadk+ "LOADK" "dst addr => load a constant from the given address in `addr'  into the register denoted by `dst'")
  (+opcode-loadi+ "LOADI" "dst value  => load the immediate `value' into the register `dst'"))

(deftype tpe-register ()
  '(integer 0 *))

(deftype tpe-address ()
  '(integer 0 *))

;; TODO: make this a go-value
(deftype tpe-immediate () t)

(deftype tpe-operand ()
  '(or tpe-register tpe-immediate tpe-address))

;; We use fixed amount of operands to have all packed into a single vector
;; improving cache locality
(defstruct (instruction (:conc-name instr-) (:constructor instr (provided-opcode &optional o1 o2 o3)))
  (opcode provided-opcode :type tpe-opcode :read-only t)
  (op1 o1  :type tpe-operand :read-only t)
  (op2 o2 :type tpe-operand :read-only t)
  (op3 o3 :type tpe-operand :read-only t))

;; let's define a couple of convenience constructors
;; which can be inlined easily by the compiler
(-> nop () instruction)
(defun nop ()
  (instr +opcode-nop+))

(-> ret () instruction)
(defun ret ()
  (instr +opcode-ret+))

(-> halt () instruction)
(defun halt ()
  (instr +opcode-halt+))

(-> brk () instruction)
(defun brk ()
  (instr +opcode-brk+))

(-> mov (tpe-register tpe-register) instruction)
(defun mov (dst src)
  (instr +opcode-mov+ dst src))

(-> loadi (tpe-register tpe-immediate) instruction)
(defun loadi (dst value)
  (instr +opcode-loadi+ dst value))

(-> loadk (tpe-register tpe-address) instruction)
(defun loadk (dst addr)
  (instr +opcode-loadk+ dst addr))
