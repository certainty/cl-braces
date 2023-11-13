(in-package :cl-braces.compiler)

(defun debug-compiler (&optional (debug t))
  (if debug
      (push :cl-braces-debug-compiler *features*)
      (setf *features* (remove :cl-braces-debug-compiler *features*))))

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

    #+cl-braces-debug-compiler
    (progn
      (format t "Parsing stage finished with ~A errors~%" (length (parser:parse-errors state)))
      (ast:print-ast ast))

    (codegen:generate-chunk ast)))
