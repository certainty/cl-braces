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

(defun format-span (span stream)
  (format stream " [~A:~A ~A:~A]"
          (token:location-line (span-from span))
          (token:location-column (span-from span))
          (token:location-line (span-to span))
          (token:location-column (span-to span))))

(defun connective (indentation-level)
  (let ((connection "├─── "))
    (cond
      ((zerop indentation-level) "")
      ((= 1 indentation-level) connection)
      (t (let ((indentation ""))
           (dotimes (i (1- indentation-level))
             (setf indentation (concatenate 'string indentation "│    ")))
           (format nil "~A~A" indentation connection))))))

(defmethod enter ((printer ast-printer) (node node))
  (with-slots (indentation-level stream print-spans-p) printer
    (format stream "~A~A" (connective indentation-level) (class-name (class-of node)))
    (when print-spans-p
      (format-span (span node) stream))
    (terpri stream)))

(defmethod leave ((printer ast-printer) (node node))
  nil)

(defmethod enter ((printer ast-printer) (node bad-expression))
  (with-slots (stream print-spans-p) printer
    (format stream "~A~A" (indent printer) "<bad-expression>")
    (when print-spans-p
      (format-span (span node) stream))
    (terpri stream)))

(defmethod enter ((printer ast-printer) (node literal))
  (with-slots (stream print-spans-p indentation-level) printer
    (with-slots (token) node
      (format stream "~A~A" (connective indentation-level) (token:lexeme token))
      (when print-spans-p
        (format-span (span node) stream))
      (terpri stream))))

(defmethod enter ((printer ast-printer) (node unary-expression))
  (with-slots (indentation-level stream print-spans-p) printer
    (with-slots (operator operand) node
      (format stream "~A~A"  (connective indentation-level) (token:lexeme operator))
      (when print-spans-p
        (format-span (span node) stream))
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
        (format-span (span node) stream))
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
        (format-span (span node) stream))
      (terpri stream)
      (incf indentation-level))))

(defmethod leave ((printer ast-printer) (node grouping-expression))
  (with-slots (indentation-level) printer
    (decf indentation-level)))
