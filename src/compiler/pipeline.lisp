(in-package :cl-braces.compiler)

(defun compile-this (input-designator)
  "Compile the `input-designator' to a chunk of bytecode."
  (multiple-value-bind (ast had-errors state) (parser:parse input-designator)
    (declare (ignore state))
    (unless had-errors
      (codegen:generate-chunk ast))))
