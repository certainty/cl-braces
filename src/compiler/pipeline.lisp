(in-package :cl-braces.compiler)

(defun compile-this (input-designator)
  (multiple-value-bind (ast had-errors state) (parser:parse input-designator)
    (declare (ignore state))
    (unless had-errors
      (generate-code ast))))
