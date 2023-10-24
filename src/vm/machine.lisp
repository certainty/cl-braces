(in-package :cl-braces/vm)

(defparameter *vm* nil "The virtual machine executing")
(defparameter *stack-size* 32 "The stacksize of the machine")
(defparameter *allocate-registers* 16 "The number of registers to allocate initially")
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

(-> mov (tpe-register tpe-register) instruction)
(defun mov (dst src)
  (instr +opcode-mov+ dst src))

(-> loadi (tpe-register tpe-immediate) instruction)
(defun loadi (dst value)
  (instr +opcode-loadi+ dst value))

(-> loadk (tpe-register tpe-address) instruction)
(defun loadk (dst addr)
  (instr +opcode-loadk+ dst addr))

(defstruct (chunk (:conc-name chunk-) (:constructor chunk (&rest provided-instructions)))
  (instructions (make-array (length provided-instructions) :element-type 'instruction :initial-contents provided-instructions) :type (vector instruction *) :read-only t))

(defstruct (call-frame (:constructor make-call-frame (&key (allocate-registers 15))))
  (registers (make-array allocate-registers :initial-element 0)))

(defstruct (virtual-machine (:conc-name vm-) (:constructor make-vm (&key (stack-size *stack-size*) (allocate-registers *allocate-registers*))))
  (stack-size stack-size :read-only t)
  (allocate-registers allocate-registers :read-only t)
  (registers (make-array allocate-registers :initial-element 0 :adjustable t))
  (call-stack (make-array stack-size :initial-element nil :element-type '(or null call-frame)))
  (instruction-pointer 0))

;; TODO: add compile time exhaustiveness check
(defmacro opcode-case (opcode &rest executions)
  (let ((next-opcode (gensym)))
    `(let ((,next-opcode ,opcode))
       (cond
         ,@(loop for exec in executions collect `((= ,next-opcode ,(first exec)) ,@(cdr exec)))
         (t (error "VM bug"))))))

(defvar *test-chunk*
  (chunk
   (loadi 0 10)
   (mov 1 0)
   (halt)))

(defun disass (chunk)
  (loop
    for addr from 0
    for instruction across (chunk-instructions chunk)
    do (disass-instruction instruction addr)))

(defun disass-instruction (instruction addr &optional (stream *standard-output*))
  (format stream "~4,'0d: 0x~X ~6a ~@[~a~] ~@[~a~] ~@[~a~] ~%" addr (instr-opcode instruction) (aref *mnemonics* (instr-opcode instruction)) (instr-op1 instruction) (instr-op2 instruction) (instr-op3 instruction)))

(defun disass-operand (operand)
  ;; keep it simple for now
  (typecase operand
    (operand-register (format nil "R~d" (reg-value operand)))
    (operand-immediate (format nil "I~d" (immediate-value operand)))))

(defun reset! (vm)
  (setf (vm-instruction-pointer vm) 0)
  (setf (vm-call-stack vm) (make-array (vm-stack-size vm) :initial-element nil :element-type '(or null call-frame)))
  (setf (vm-registers vm)  (make-array (vm-allocate-registers vm) :initial-element 0 :adjustable t))
  nil)

(defun execute-chunk (vm chunk &key (entry-point 0))
  (let* ((*vm* vm)
         (*current-chunk* chunk)
         (instructions (chunk-instructions chunk)))
    (setf (vm-instruction-pointer vm) entry-point)
    (loop
      (let ((instruction (aref instructions (vm-instruction-pointer vm))))
        (incf (vm-instruction-pointer vm))
        (opcode-case (instr-opcode instruction)
                     (+opcode-nop+ (continue))
                     (+opcode-halt+ (return-from execute-chunk))
                     (+opcode-call+ (continue))
                     (+opcode-ret+ (continue))
                     (+opcode-move+
                      (let* ((operands (instr-operands instruction))
                             (dst (aref operands 0))
                             (src (aref operands 1)))
                        (setf (aref (vm-registers vm)  (reg-value dst)) (reg-value src))))
                     (+opcode-loadk+ (continue)))))))
