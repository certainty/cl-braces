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
  (+opcode-call+ "CALL" "call r0 r1 r2 - call a function, r0 is the function, r1 is the number of arguments, r2 is the number of return values")
  (+opcode-ret+ "RED" "ret - return from a function")
  (+opcode-move+ "MOV" "mov dst src - move between registers")
  (+opcode-loadk+ "LOADK" "loadk dst k - load a constant into a register "))

(deftype tpe-register ()
  '(integer 0 *))

(deftype tpe-immediate () t)

(deftype tpe-operand ()
  '(or tpe-register tpe-immediate))

(defmacro reg (value)
  `(progn ,value))

(defmacro imm (value)
  `(progn ,value))

(defstruct (instruction (:conc-name instr-) (:constructor instr (provided-opcode &rest provided-operands)))
  (opcode provided-opcode :type tpe-opcode :read-only t)
  (operands (coerce provided-operands 'vector) :type (vector tpe-operand *) :read-only t))

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
   (instr +opcode-loadk+ (reg 0) (imm 1))
   (instr +opcode-move+  (reg 1) (reg 0))
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
