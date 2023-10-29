(in-package :cl-braces.vm)

(defparameter *vm* nil "The virtual machine executing")
(defparameter *stack-size* 32 "The stacksize of the machine")
(defparameter *allocate-registers* 16 "The number of registers to allocate initially")
(defparameter *current-chunk* nil "The currently executed chunk")
(defparameter *current-stack-frame* nil "The currently executed stackframe")

(defstruct (call-frame (:constructor make-call-frame (provided-code &key (allocate-registers *allocate-registers*))))
  (code provided-code :type bytecode:chunk :read-only t)
  (registers (make-array allocate-registers :initial-element 0)))

(defstruct (virtual-machine (:conc-name vm-) (:constructor make-vm (&key (stack-size *stack-size*) (allocate-registers *allocate-registers*))))
  (stack-size stack-size :read-only t)
  (allocate-registers allocate-registers :read-only t)
  (registers (make-array allocate-registers :initial-element 0 :adjustable t))
  (call-stack (make-array stack-size :initial-element nil :element-type '(or null call-frame)))
  (instruction-pointer 0 :type fixnum))

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
        (bytecode:opcode-case (bytecode:instr-opcode instruction)
                              (bytecode:+opcode-nop+ (continue))
                              (bytecode:+opcode-halt+ (return-from execute-chunk))
                              (bytecode:+opcode-call+ (continue))
                              (bytecode:+opcode-ret+ (continue))
                              (bytecode:+opcode-brk+
                               (break "<Breakpoint> IP: 0x~X" (vm-instruction-pointer vm)))
                              (bytecode:+opcode-mov+
                               (let ((dst (bytecode:instr-op1 instruction))
                                     (src (bytecode:instr-op2 instruction)))
                                 (setf (aref (vm-registers vm) dst) src)))
                              (bytecode:+opcode-loadi+
                               (let ((dst (bytecode:instr-op1 instruction))
                                     (val (bytecode:instr-op2 instruction)))
                                 (setf (aref (vm-registers vm) dst) val)))
                              (bytecode:+opcode-loadk+ (continue)))))))
