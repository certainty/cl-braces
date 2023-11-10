(in-package :cl-braces.vm.bytecode)

(deftype opcode-t ()  '(unsigned-byte 8))
(deftype register-t () '(unsigned-byte 16))
(deftype address-t () '(unsigned-byte 64))
(deftype operand-t () '(or register-t address-t))

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

(-> version ((integer 0 *) (integer 0 *)) version)
(defstruct (version (:constructor version (major minor)))
  (major 0 :type (integer 0 *) :read-only t)
  (minor 0 :type (integer 0 *) :read-only t))

(defclass isa ()
  ((version
    :reader isa-version
    :initarg :version
    :initform (version 1 0 )
    :type version)
   (instruction-set
    :reader isa-instruction-set
    :initarg :instructions
    :type (vector isa-instruction))
   (instructions-by-mnemonic
    :initform (make-hash-table :test 'eql)
    :type (hash-table symbol isa-instruction)))
  (:documentation "A description of an instruction set architecture"))

(defmethod initialize-instance :after ((isa isa) &key)
  (with-slots (instructions-by-mnemonic instruction-set) isa
    (loop :for i :across instruction-set
          :do (setf (gethash (isa-instruction-mnemonic i) instructions-by-mnemonic) i))))

(defgeneric print-isa (isa &optional stream)
  (:documentation "Prints the given isa to the stream"))

(defmethod print-isa ((isa isa) &optional (stream t))
  (format stream "Version: ~A.~A ~%Instructions:~% ~{~a~^~% ~}" (version-major (isa-version isa)) (version-minor (isa-version isa))
          (mapcar (lambda (i) (print-isa i nil)) (coerce (isa-instruction-set isa) 'list))))

(defun instruction-by-mnemonic (mnemonic &optional (isa *current-isa*))
  (with-slots (instructions-by-mnemonic) isa
    (let ((instruction (gethash mnemonic instructions-by-mnemonic)))
      (unless instruction
        (error "Unknown instruction ~A" mnemonic))
      instruction)))

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

(defmethod print-isa ((operand isa-operand) &optional (stream t))
  (with-slots (name) operand
    (format stream "~A" name)))

(defclass isa-instruction ()
  ((opcode
    :reader isa-instruction-opcode
    :initarg
    :opcode
    :type opcode-t)
   (operands
    :reader isa-instruction-operands
    :initarg
    :operands
    :type (vector isa-operand))
   (mnemonic
    :reader isa-instruction-mnemonic
    :initarg :mnemonic
    :type string)
   (description
    :initarg
    :description
    :type string))
  (:documentation "A description of an instruction that can be used to construct instructions"))

(defmethod print-isa ((instruction isa-instruction) &optional (stream t))
  (with-slots (mnemonic operands description) instruction
    (let ((printed-operands (format nil "~{~a~^, ~}" (mapcar (lambda (op) (print-isa op nil)) (coerce operands 'list)))))
      (format stream "~7a ~18a ~a" mnemonic printed-operands description))))

(defmacro define-isa (isa-name  &key version instructions)
  "Defines a new isa with the given version and instructions."
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

(defparameter *current-isa* "The current isa that is used to construct instructions")

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

;; A machine instruction as loaded and executed by the vm.
;; We chose a representation that is easy to decode and cache-friendly.
(defstruct instruction
  (opcode (error "must supply opcode") :type opcode-t :read-only t))

(defstruct (unary-instruction (:include instruction))
  (op1 (error "must supply op1") :type operand-t :read-only t))

(defstruct (binary-instruction (:include unary-instruction))
  (op2 (error "must supply op2") :type operand-t :read-only t))

(defstruct (ternary-instruction (:include binary-instruction))
  (op3 (error "must supply op3") :type operand-t :read-only t))

(deftype constant-table () '(vector value:value))

(defstruct chunk
  (constants (error "must supply constants") :type (vector value:value) :read-only t)
  (code (error "must supply code") :type (vector instruction) :read-only t)
  (registers-used 0 :type (integer 0 *) :read-only t))
