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
    (with-preorder-traversal
      (walk printer ast))))

(defmethod development:debug-print ((obj ast:node) &key (stream *standard-output*))
  (print-ast obj :stream stream :print-spans-p nil))

(defun format-span (span stream)
  (format stream " [~A:~A ~A:~A]"
          (location:line (location:span-from span))
          (location:column (location:span-from span))
          (location:line (location:span-to span))
          (location:column (location:span-to span))))

(defun connective (indentation-level)
  (let ((connection "├─── "))
    (cond
      ((zerop indentation-level) "")
      ((= 1 indentation-level) connection)
      (t (let ((indentation ""))
           (dotimes (i (1- indentation-level))
             (setf indentation (concatenate 'string indentation "│    ")))
           (format nil "~A~A" indentation connection))))))

;; default implementations
(defmethod enter ((printer ast-printer) (node node))
  (with-slots (indentation-level stream print-spans-p) printer
    (format stream "~A~A" (connective indentation-level) (class-name (class-of node)))
    (when print-spans-p
      (format-span (location:span-for node) stream))
    (terpri stream)))

(defmethod leave ((printer ast-printer) (node node))
  nil)

(defmethod enter ((printer ast-printer) (node program))
  (with-slots (stream print-spans-p indentation-level) printer
    (with-slots (declarations) node
      (format stream "~A~A" (connective indentation-level) "program")
      (when print-spans-p
        (format-span (location:span-for node) stream))
      (incf indentation-level)
      (terpri stream))))

(defmethod leave ((printer ast-printer) (node program))
  (with-slots (indentation-level) printer
    (decf indentation-level)))

(defmethod enter ((printer ast-printer) (node ast:block))
  (with-slots (stream print-spans-p indentation-level) printer
    (with-slots (statements) node
      (format stream "~A~A" (connective indentation-level) "block")
      (when print-spans-p
        (format-span (location:span-for node) stream))
      (incf indentation-level)
      (terpri stream))))

(defmethod enter ((printer ast-printer) (node ast:if-statement))
  (with-slots (stream print-spans-p indentation-level) printer
    (format stream "~A~A" (connective indentation-level) "if-statement")
    (when print-spans-p
      (format-span (location:span-for node) stream))
    (incf indentation-level)
    (terpri stream)))

(defmethod leave ((printer ast-printer) (node ast:block))
  (with-slots (indentation-level) printer
    (decf indentation-level)))

;; specific implementations

(defmethod enter ((printer ast-printer) (node literal))
  (with-slots (stream print-spans-p indentation-level) printer
    (with-slots (token) node
      (format stream "~A~A" (connective indentation-level) (token:lexeme token))
      (when print-spans-p
        (format-span (location:span-for node) stream))
      (terpri stream))))

(defmethod enter ((printer ast-printer) (node unary-expression))
  (with-slots (indentation-level stream print-spans-p) printer
    (with-slots (operator operand) node
      (format stream "~A~A"  (connective indentation-level) (token:lexeme operator))
      (when print-spans-p
        (format-span (location:span-for node) stream))
      (terpri stream)
      (incf indentation-level))))

(defmethod leave ((printer ast-printer) (node unary-expression))
  (with-slots (indentation-level) printer
    (decf indentation-level)))

(defmethod enter ((printer ast-printer) (node binary-expression))
  (with-slots (indentation-level stream print-spans-p) printer
    (with-slots (operator left right) node
      (format stream "~A~A"  (connective indentation-level) (token:lexeme operator))
      (when print-spans-p
        (format-span (location:span-for node) stream))
      (terpri stream)
      (incf indentation-level))))

(defmethod leave ((printer ast-printer) (node binary-expression))
  (with-slots (indentation-level) printer
    (decf indentation-level)))

(defmethod enter ((printer ast-printer) (node grouping-expression))
  (with-slots (indentation-level stream print-spans-p) printer
    (with-slots (expression) node
      (format stream "~A~A"  (connective indentation-level) "()")
      (when print-spans-p
        (format-span (location:span-for node) stream))
      (terpri stream)
      (incf indentation-level))))

(defmethod leave ((printer ast-printer) (node grouping-expression))
  (with-slots (indentation-level) printer
    (decf indentation-level)))

(defmethod enter ((printer ast-printer) (node bad-declaration))
  (with-slots (stream print-spans-p indentation-level) printer
    (format stream "~A~A" (connective indentation-level) "bad-declaration")
    (when print-spans-p
      (format-span (location:span-for node) stream))
    (terpri stream)))

(defmethod enter ((printer ast-printer) (node expression-statement))
  (with-slots (indentation-level stream print-spans-p) printer
    (with-slots (expression) node
      (format stream "~A~A"  (connective indentation-level) "expression-statement")
      (when print-spans-p
        (format-span (location:span-for node) stream))
      (terpri stream)
      (incf indentation-level))))

(defmethod leave ((printer ast-printer) (node expression-statement))
  (with-slots (indentation-level) printer
    (decf indentation-level)))

(defmethod enter ((printer ast-printer) (node short-variable-declaration))
  (with-slots (indentation-level stream print-spans-p) printer
    (with-slots (identifier initializer) node
      (format stream "~A~A"  (connective indentation-level) "short-variable-declaration")
      (when print-spans-p
        (format-span (location:span-for node) stream))
      (terpri stream)
      (incf indentation-level))))

(defmethod leave ((printer ast-printer) (node short-variable-declaration))
  (with-slots (indentation-level) printer
    (decf indentation-level)))

(defmethod enter ((printer ast-printer) (node variable))
  (with-slots (indentation-level stream print-spans-p) printer
    (with-slots (identifier type initializer) node
      (format stream "~A~A"  (connective indentation-level) (token:lexeme identifier))
      (when print-spans-p
        (format-span (location:span-for node) stream))
      (terpri stream))))

(defmethod enter ((printer ast-printer) (node identifier))
  (with-slots (indentation-level stream print-spans-p) printer
    (format stream "~A~A"  (connective indentation-level) (identifier-name node))
    (when print-spans-p
      (format-span (location:span-for node) stream))
    (terpri stream)))

(defmethod enter ((printer ast-printer) (node statement-list))
  (with-slots (indentation-level stream print-spans-p) printer
    (format stream "~A~A"  (connective indentation-level) "statement-list")
    (when print-spans-p
      (format-span (location:span-for node) stream))
    (terpri stream)
    (incf indentation-level)))

(defmethod leave ((printer ast-printer) (node statement-list))
  (with-slots (indentation-level) printer
    (decf indentation-level)))

(defmethod enter ((printer ast-printer) (node expression-list))
  (with-slots (indentation-level stream print-spans-p) printer
    (format stream "~A~A"  (connective indentation-level) "expression-list")
    (when print-spans-p
      (format-span (location:span-for node) stream))
    (terpri stream)
    (incf indentation-level)))

(defmethod leave ((printer ast-printer) (node expression-list))
  (with-slots (indentation-level) printer
    (decf indentation-level)))

(defmethod enter ((printer ast-printer) (node identifier-list))
  (with-slots (indentation-level stream print-spans-p) printer
    (format stream "~A~A"  (connective indentation-level) "identifier-list")
    (when print-spans-p
      (format-span (location:span-for node) stream))
    (terpri stream)
    (incf indentation-level)))

(defmethod leave ((printer ast-printer) (node identifier-list))
  (with-slots (indentation-level) printer
    (decf indentation-level)))
