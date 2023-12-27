(in-package :cl-braces.bytecode)

;;;; Here we define the in-memory representation of the bytecode instructions.
;;;; The objectvie here is use a representation that is cache friendly and easy to decode.
;;;;
;;;; The main artifact that exposed by thie is what we call a `chunk'.
;;;; A `chunk' is a collection of instructions and constants that are loaded into the VM.
;;;; This doesn't make any assumptions about how the compiler choses to reprsent the
;;;; compiled program. It could generate a chunk for the whole propgram or follow a chunk per function approach.

(deftype opcode-t ()  '(unsigned-byte 8))
(deftype register-t () '(unsigned-byte 16))
(deftype address-t () '(unsigned-byte 64))
(deftype label-t () '(unsigned-byte 64))
(deftype operand-t () '(or register-t address-t label-t))

;; A machine instruction as loaded and executed by the vm.
(s:defconstructor instruction
  (opcode opcode-t)
  (operands (vector operand-t)))

(defmethod print-object ((instruction instruction) stream)
  (print-unreadable-object (instruction stream :type nil)
    (format stream "INSTRUCTION OPCODE: ~A ~A" (instruction-opcode instruction) (instruction-operands instruction))))

(deftype constant-table () '(vector runtime.value:value))

(s:defunion arity
  (arity-exactly (n fixnum))
  (arity-at-least (n fixnum)))

(defclass function-record ()
  ((arity
    :reader function-record-arity
    :initarg :arity
    :type arity)
   (registers-used
    :reader function-record-registers-used
    :initarg :registers-used
    :type (integer 0 *))
   (address
    :reader function-record-address
    :initarg :address
    :type address-t)))

(defclass chunk ()
  ((constants
    :reader chunk-constants
    :initarg :constants
    :initform (make-array 0  :element-type 'runtime.value:value :adjustable t :fill-pointer t)
    :type  constant-table)
   (functions
    :reader chunk-functions
    :initarg :functions
    :initform (make-array 0 :element-type 'function-record :adjustable t :fill-pointer t)
    :type (vector function-record))
   (code
    :reader chunk-code
    :initarg :code
    :initform (make-array 0 :element-type 'instruction :adjustable t :fill-pointer t)
    :type (vector instruction))
   (block-labels
    :reader chunk-block-labels
    :initarg :block-labels
    :initform (make-hash-table :test #'equalp)
    :type hash-table)
   (registers-used
    :reader chunk-registers-used
    :initarg :registers-used
    :initform 0
    :type (integer 0 *))
   (entrypoint
    :reader chunk-entrypoint
    :initarg :entrypoint
    :initform 0
    :type address-t)))

(defmacro do-instructions ((pc instruction chunk) &body body)
  `(with-slots (code) chunk
     (loop for ,pc from 0 below (length code) do
       (let ((,instruction (aref code ,pc)))
         ,@body))))
