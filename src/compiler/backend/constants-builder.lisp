(in-package :cl-braces.compiler.backend.codegen)

(defclass constants-builder ()
  ((registered-constants
    :initform (make-hash-table :test 'equal)
    :type (hash-table bytecode:address vm.value:value)
    :documentation "A hash table that maps constant values to their addresses in the constant table")
   (next-address
    :initform 0
    :type bytecode:address-t
    :documentation "The next index to use for a constant in the constant table"))
  (:documentation "A builder that allows to generate deduplicated constant segments for code chunks"))

(defun make-constants-builder ()
  (make-instance 'constants-builder))

(-> constants-add (constants-builder vm.value:value) bytecode:address)
(defun constants-add (builder value)
  "Add the constant to the constant pool and return its address"
  (with-slots (registered-constants next-address) builder
    (or (gethash value registered-constants)
        (let ((address (bytecode:addr next-address)))
          (prog1 address
            (setf (gethash value registered-constants) address)
            (incf next-address))))))

(-> constants-result (constants-builder) bytecode:constant-table)
(defun constants-result (builder)
  "Builds the final constant table, which can be used in code chunks."
  (with-slots (registered-constants) builder
    (let* ((constants registered-constants)
           (constant-table (make-array (hash-table-count constants))))
      (loop :for constant being the hash-key using (hash-value adress) of constants
            :do
               (setf (aref constant-table (bytecode:operand-value adress)) constant)
            :finally (return constant-table)))))
