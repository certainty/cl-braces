(in-package :cl-braces.compiler.frontend.ast)

(defclass ast-printer ()
  ((indentation-level
    :initarg :indentation-level
    :initform 0
    :type integer
    :documentation "The indentation level")
   (print-spans-p
    :initarg :print-spans-p
    :initform nil
    :type boolean)
   (stream
    :initarg :stream
    :initform *standard-output*
    :type stream)))


(defun print-ast (ast &key (stream *standard-output*) (print-spans-p nil))
  "Prints the give `AST' to  `STREAM' in a human readable format."
  (let ((printer (make-instance 'ast-printer :stream stream :print-spans-p print-spans-p)))
    (walk printer ast)))

(defmethod support:debug-print ((obj ast:node))
  (print-ast obj :stream *debug-io* :print-spans-p nil))

(defun format-span (span stream)
  (format stream " [~A:~A ~A:~A]"
          (location:line (span:from span))
          (location:column (span:from span))
          (location:line (span:to span))
          (location:column (span:to span))))

(defun connective (indentation-level)
  (let ((connection "├─── "))
    (cond
      ((zerop indentation-level) "")
      ((= 1 indentation-level) connection)
      (t (let ((indentation ""))
           (dotimes (i (1- indentation-level))
             (setf indentation (concatenate 'string indentation "│    ")))
           (format nil "~A~A" indentation connection))))))

(defun print-node (printer node caption &key (leafp nil))
  (with-slots (indentation-level stream print-spans-p) printer
    (format stream "~A~A" (connective indentation-level) caption)
    (when print-spans-p
      (format-span (span:for node) stream))
    (terpri stream)
    (unless leafp
      (incf indentation-level))))

(defmacro define-default-printer (node caption &key (leafp nil))
  (let ((nodevar (gensym))
        (printervar (gensym)))

    `(progn
       (defmethod enter ((,printervar ast-printer) (,nodevar ,node))
         (print-node ,printervar ,nodevar ,caption :leafp ,leafp))
       ,(unless leafp
          `(defmethod leave ((,printervar ast-printer) (,nodevar ,node))
             (declare (ignore ,nodevar))
             (decf (slot-value ,printervar 'indentation-level)))))))

;; default implementations

(defmethod enter ((printer ast-printer) (node node))
  (with-slots (indentation-level stream print-spans-p) printer
    (format stream "~A~A" (connective indentation-level) (class-name (class-of node)))
    (when print-spans-p
      (format-span (span:for node) stream))
    (terpri stream)))

(defmethod leave ((printer ast-printer) (node node))
  nil)

(define-default-printer source-file "source-file")
(define-default-printer block "block")
(define-default-printer if-statement "if-statement")
(define-default-printer empty-statement "empty-statement")
(define-default-printer literal "literal")
(define-default-printer unary-expression "unary-expression")
(define-default-printer binary-expression "binary-expression")
(define-default-printer grouping-expression "grouping-expression")
(define-default-printer bad-statement "bad-statement" :leafp t)
(define-default-printer expression-statement "expression-statement")
(define-default-printer short-variable-declaration "short-variable-declaration")
(define-default-printer variable-declaration "variable-declaration")
(define-default-printer variable "variable")
(define-default-printer identifier "identifier")
(define-default-printer statement-list "statement-list")
(define-default-printer expression-list "expression-list")
(define-default-printer identifier-list "identifier-list")
(define-default-printer type-specifier "type-specifier")
(define-default-printer variable-specification "variable-specification")
(define-default-printer assignment-statement "assignment-statement")

(define-default-printer function-declaration "function-declaration")
(define-default-printer function-signature "function-signature")
(define-default-printer parameter-list "parameter-list")
(define-default-printer parameter-declaration "parameter-declaration")
(define-default-printer parameter-splat "parameter-splat")
(define-default-printer package-declaration "package-declaration")

(defmethod enter ((printer ast-printer) (tok token:token))
  (with-slots (indentation-level stream print-spans-p) printer
    (format stream "~A~A"  (connective indentation-level) (string-upcase (token:lexeme tok)))
    (when print-spans-p
      (format-span (span:for tok) stream))
    (terpri stream)))

(defmethod leave ((printer ast-printer) (tok token:token))
  nil)
