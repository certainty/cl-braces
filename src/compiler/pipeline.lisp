(in-package :cl-braces.compiler)

(define-condition compile-error (error)
  ((message :initarg :message :reader compile-error-message)
   (details :initarg :details :reader compile-error-details))
  (:report (lambda (condition stream)
             (format stream "~A ~A"
                     (compile-error-message condition)
                     (compile-error-details condition)))))

(defun compile-this (input-designator)
  "Compile the `input-designator' to a chunk of bytecode."
  (multiple-value-bind (ast had-errors state) (parser:parse input-designator)
    (when had-errors
      (error (make-condition 'compile-error :message "Compilation failed" :details (parser:parse-errors state))))
    (codegen:generate-chunk ast)))
