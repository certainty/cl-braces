(in-package :cl-braces.compiler)

(define-condition compile-error (error)
  ((message :initarg :message :reader compile-error-message)
   (details :initarg :details :reader compile-error-details))
  (:report (lambda (condition stream)
             (format stream "~A" (compile-error-message condition)))))

(defun compile-this (input-designator)
  "Compile the `input-designator' to a chunk of bytecode."
  (multiple-value-bind (ast had-errors state) (parser:parse input-designator)
    (when had-errors
      (signal 'compile-error :message "Parse error" :details (parser:parse-errors state)))
    (codegen:generate-chunk ast)))
