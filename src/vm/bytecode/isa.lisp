(in-package :cl-braces.vm.bytecode)

;;; The instruction set architecture (ISA) is a description of the instructions that are available in the virtual machine.
;;; It is used to construct instructions and to print the instructions in a human readable format.
;;; It also serves as the basis for the disassembler and the code generator so we have a single source of truth for the instructions.
;;;
;;; The ISA is defined using the define-isa macro, which is used to describe and define a new instruction set.
;;;
;;; Use the `print-isa' function to get information about the instructions that are available in the virtual machine.
;;; The default is will be bound to `*current-isa*' so you can use `(print-isa *current-isa*)' to get information about the current instruction set.

(defparameter *current-isa* "The current isa that is used to construct instructions")

(-> version ((integer 0 *) (integer 0 *)) version)
(defstruct (version (:constructor version (major minor)))
  (major 0 :type (integer 0 *) :read-only t)
  (minor 0 :type (integer 0 *) :read-only t))

(defclass isa ()
  ((version
    :reader isa-version
    :initarg :version
    :initform (version 1 0 )
    :type version
    :documentation "The version of the instruction set architecture")
   (instruction-set
    :reader isa-instruction-set
    :initarg :instructions
    :type (vector isa-instruction)
    :documentation "The instructions that are available in this instruction set architecture")
   (instructions-by-mnemonic
    :initform (make-hash-table :test 'eql)
    :type (hash-table symbol isa-instruction)))
  (:documentation "This class represents an instruction set architecture (ISA). It's used in code-generation and disassembly to describe the instructions that are available in the virtual machine."))

(defmethod initialize-instance :after ((isa isa) &key)
  (with-slots (instructions-by-mnemonic instruction-set) isa
    (loop :for i :across instruction-set
          :do (setf (gethash (isa-instruction-mnemonic i) instructions-by-mnemonic) i))))

(defun instruction-by-mnemonic (mnemonic &optional (isa *current-isa*))
  "Returns the instruction with the given mnemonic"
  (with-slots (instructions-by-mnemonic) isa
    (gethash mnemonic instructions-by-mnemonic)))

(defun instruction-by-opcode (opcode &optional (isa *current-isa*))
  "Returns the instruction with the given opcode."
  (with-slots (instruction-set) isa
    (loop :for i :across instruction-set
          :when (eql opcode (isa-instruction-opcode i))
            :return i)))

(defun mnemonic-for-instruction (opcode &optional (isa *current-isa*))
  "Returns the mnemonic for the given opcode."
  (with-slots (instructions-by-mnemonic) isa
    (loop :for i :being :the :hash-values :of instructions-by-mnemonic
          :when (eql opcode (isa-instruction-opcode i))
            :return (isa-instruction-mnemonic i))))

;; A type that is only used during construction of instuctions to make the operations more typesafe
;; In the finaly instruction we only encode the raw value
(-> reg (register-t) register)
(defstruct (register  (:conc-name register-) (:constructor reg (provided-value)) (:copier nil))
  (value provided-value  :type register-t :read-only t))

(-> addr (integer) address)
(defstruct (address (:conc-name address-) (:constructor addr (provided-value)) (:copier nil))
  (value provided-value :type address-t :read-only t))

(defgeneric operand-value (operand)
  (:documentation "Returns the value of the given operand"))

(defmethod operand-value ((operand register))
  (register-value operand))

(defmethod operand-value ((operand address))
  (address-value operand))

(defclass isa-operand ()
  ((type-description
    :reader isa-operand-type-description
    :initarg
    :type-description
    :type (member :register :address))
   (type-guard
    :reader isa-operand-type-guard
    :initarg :type-guard
    :type symbol)
   (name :initarg :name :type string))
  (:documentation "A description of an operand type that can be used to construct instructions"))

(defun isa-operand-typep (operand expected-type)
  (typep (isa-operand-type-guard operand) expected-type))

(defun isa-operand-type (operand)
  (with-slots (type-guard) operand
    type-guard))

(defclass isa-instruction ()
  ((opcode
    :reader isa-instruction-opcode
    :initarg
    :opcode
    :type opcode-t
    :documentation "The opcode of the instruction")
   (operands
    :reader isa-instruction-operands
    :initarg
    :operands
    :type (vector isa-operand)
    :documentation "The operands that are required for this instruction")
   (mnemonic
    :reader isa-instruction-mnemonic
    :initarg :mnemonic
    :type string
    :documentation "The mnemonic of the instruction")
   (description
    :initarg
    :description
    :type string
    :documentation "A helpful description of the instruction"))
  (:documentation "A description of an instruction that can be used to construct instructions"))

(defmacro define-isa (isa-name  &key version instructions)
  "Defines a new isa with the given version and instructions.
    The instructions are defined using the following syntax:
    (define-isa *my-isa*
      :version (1 0) :
      instructions
      ((add \"ADD\" ((reg dst) (reg lhs) (reg rhs)) \"Adds two registers\"))

   Each instruction is defined using the following syntax:
   (mnemonic operands description)

   The operands are defined using the following syntax:
   (type name)

   The supported operand types are:
   - reg: A register
   - addr: An address
"
  `(progn
     (defparameter ,isa-name
       (make-instance 'isa
                      :version (version ,(first version) ,(second version))
                      :instructions (make-isa-instruction-set ,@instructions)))))

(defmacro make-isa-instruction-set (&rest instructions)
  `(vector ,@(mapcar (lambda (instruction)
                       `(make-isa-instruction ,@instruction))
                     instructions)))

(defmacro make-isa-instruction (opcode mnemonic operands description)
  `(make-instance 'isa-instruction
    :opcode ,opcode
    :mnemonic ',mnemonic
    :description ,description
    :operands (vector ,@(mapcar (lambda (operand)
                                  (case (first operand)
                                    (reg `(make-instance 'isa-operand :type-description :register :type-guard 'register :name ,(format nil "$~a" (second operand))))
                                    (addr `(make-instance 'isa-operand :type-description :address :type-guard 'address :name ,(format nil "@~a" (second operand))))
                                    (t (error "Unknown operand type ~A" (first operand)))))
                                operands))))

(defun instr (mnemonic &rest args)
  "Creates an instruction from the given mnemonic and operands."
  (let* ((instruction (instruction-by-mnemonic mnemonic))
         (opcode (isa-instruction-opcode instruction))
         (expected-operands (isa-instruction-operands instruction)))
    (check-operands expected-operands (coerce args 'vector))
    (case (length expected-operands)
      ;; todo extract values again
      (0 (make-instruction :opcode opcode))
      (1 (make-unary-instruction :opcode opcode :op1 (operand-value (first args))))
      (2 (make-binary-instruction :opcode opcode :op1 (operand-value (first args)) :op2 (operand-value (second args))))
      (3 (make-ternary-instruction :opcode opcode :op1 (operand-value (first args)) :op2 (operand-value (second args)) :op3 (operand-value (third args)))))))

(defun check-operands (wanted-operands given-operands)
  "Checkst that the operands given match the required operands defined in the instruction. Also verifies that the types match."
  (unless (= (length wanted-operands) (length given-operands))
    (error "Expected ~A operands but got ~A" (length wanted-operands) (length given-operands)))
  (loop for wanted-operand across wanted-operands
        for given-operand across given-operands
        do (unless (typep given-operand (isa-operand-type-guard wanted-operand))
             (error "Expected operand of type ~A but got ~A" (isa-operand-type-guard wanted-operand) given-operand))))

(defgeneric print-isa (isa &optional stream)
  (:documentation "Prints the given isa to the stream"))

(defmethod print-isa ((isa isa) &optional (stream t))
  (format stream "Version: ~A.~A ~%Instructions:~% ~{~a~^~% ~}" (version-major (isa-version isa)) (version-minor (isa-version isa))
          (mapcar (lambda (i) (print-isa i nil)) (coerce (isa-instruction-set isa) 'list))))

(defmethod print-isa ((instruction isa-instruction) &optional (stream t))
  (with-slots (mnemonic operands description) instruction
    (let ((printed-operands (format nil "~{~a~^, ~}" (mapcar (lambda (op) (print-isa op nil)) (coerce operands 'list)))))
      (format stream "~7a ~18a ~a" mnemonic printed-operands description))))

(defmethod print-isa ((operand isa-operand) &optional (stream t))
  (with-slots (name) operand
    (format stream "~A" name)))
