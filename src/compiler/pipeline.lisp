(in-package :cl-braces.compiler)

(define-condition compile-error (error)
  ((message :initarg :message :reader compile-error-message)
   (details :initarg :details :reader compile-error-details))
  (:report (lambda (condition stream)
             (format stream "~A ~A"
                     (compile-error-message condition)
                     (compile-error-details condition)))))

(defun compile-string (input &key (fail-fast nil) (wrap-in-main t))
  (let ((full-source (format nil "package main~% func main() {~%~A~%}" input)))
    (compile-source-file full-source :fail-fast fail-fast)))

(defun compile-source-file (input-designator &key (fail-fast nil))
  (let* ((ast     (pass-syntactic-analysis input-designator :production #'parser::<source-file :fail-fast fail-fast))
         (symbols (pass-semantic-analysis ast)))
    (pass-code-generation ast symbols)))

(defmethod pass-syntactic-analysis (input-designator &key (fail-fast nil) (production #'parser::<statement-list))
  (multiple-value-bind (ast had-errors state) (parser:parse input-designator :fail-fast fail-fast :production production)
    (when had-errors
      (error (make-condition 'compile-error :message "Syntactic analysis failed" :details (parser:parse-errors state))))

    (prog1 ast
      #-cl-braces-compiler-release
      (when ast
        (format t "~%## Parse ~%")
        (support:debug-print ast)
        (terpri)))))

(defmethod pass-semantic-analysis (ast)
  (multiple-value-bind (symbol-table errors) (symbol-resolver:resolve-symbols ast)
    (when errors
      (error (make-condition 'compile-error :message "Semantic analysis failed" :details errors)))

    (prog1 symbol-table
      #-cl-braces-compiler-release
      (when symbol-table
        (format t "~%## Symbols ~%")
        (support:debug-print symbol-table)))))

(defmethod pass-code-generation (ast symbol-table)
  (let ((chunk (codegen:generate-chunk ast symbol-table)))
    (prog1 chunk
      #-cl-braces-compiler-release
      (progn
        (format t "~%## Bytecode ~%")
        (support:debug-print chunk)))))
