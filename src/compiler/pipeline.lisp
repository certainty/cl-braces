(in-package :cl-braces.compiler)

(define-condition compile-error (error)
  ((message :initarg :message :reader compile-error-message)
   (details :initarg :details :reader compile-error-details)
   (input :initarg :input :reader compile-error-input))
  (:report (lambda (condition stream)
             (format stream "~A ~A"
                     (compile-error-message condition)
                     (compile-error-details condition)))))

(defgeneric compile (input-designator &key fail-fast  &allow-other-keys)
  (:documentation "Compile the input into a bytecode chunk."))

(defmethod compile ((input string) &key (fail-fast nil) (wrap-in-main t))
  (let ((full-source (if wrap-in-main (format nil "package main~% func main() {~%~A~%}" input) input)))
    (sourcecode:with-input (source full-source)
      (compile source :fail-fast fail-fast))))

(defmethod compile ((input pathname) &key (fail-fast nil))
  (sourcecode:with-input (source input)
    (compile source :fail-fast fail-fast)))

(defmethod compile ((input sourcecode:source-input) &key (fail-fast nil))
  (let* ((ast     (pass-syntactic-analysis input :production #'parser::<source-file :fail-fast fail-fast))
         (symbols (pass-semantic-analysis ast input)))
    (pass-code-generation ast symbols)))

(defmethod pass-syntactic-analysis (input &key (fail-fast nil) (production #'parser::<statement-list))
  (multiple-value-bind (ast had-errors state) (parser:parse (sourcecode::input-string input) :fail-fast fail-fast :production production)
    (when had-errors
      (error (make-condition 'compile-error :message "Syntactic analysis failed" :details (parser:parse-errors state) :input input)))
    ast))

(defmethod pass-semantic-analysis (ast input)
  (multiple-value-bind (symbol-table errors) (symbol-resolver:resolve-symbols ast)
    (when errors
      (error (make-condition 'compile-error :message "Semantic analysis failed" :details errors :input input)))
    symbol-table))

(defmethod pass-code-generation (ast symbol-table)
  (let ((chunk (codegen:generate-chunk ast symbol-table)))
    chunk))
