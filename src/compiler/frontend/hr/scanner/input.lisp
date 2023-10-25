(in-package :cl-braces.compiler.frontend.scanner)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Create a source input for the scanner.
;; The source input is a thin wrapper that combines the source code as a stream
;; together with information about the origin of the source code.
;;
;; Examples:
;;
;; (call-with-source-input #P"foo.go" (lambda (input) ...))  ; Create a source-input from the file foo.go
;;
;; (call-with-source-input "package foo\n func main() { return 0 }" (lambda (input) ...)) ; Create a source-input from a string
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass source-origin () ()
  (:documentation "An origin for source code"))

(defclass file-origin (source-origin)
  ((path :initarg :path :reader file-origin-path))
  (:documentation "Source code originating from a file."))

(defclass string-origin (source-origin)
  ((label :initarg :label :reader string-origin-label))
  (:documentation "Source code originating from a string."))

(defgeneric source-uri (origin)
  (:documentation "Return the URI of the source origin."))

(defmethod source-uri ((origin file-origin))
  (format nil "file://~A" (file-origin-path origin)))

(defmethod source-uri ((origin string-origin))
  (format nil "string://~A" (string-origin-label origin)))

(defclass source-input ()
  ((origin :initarg :origin :reader source-input-origin :type source-origin)
   (stream :initarg :stream :reader source-input-stream :type fundamental-character-stream)))

(defgeneric source-input-open (origin)
  (:documentation "Open the source input."))

(defgeneric source-input-close (source-input)
  (:documentation "Close the source input."))

(defmethod source-input-open ((file pathname))
  (make-instance 'source-input
                 :origin (make-instance 'file-origin :path file)
                 :stream (open file :direction :input :element-type 'character)))

(defmethod source-input-open ((buffer string))
  (make-instance 'source-input
                 :origin (make-instance 'string-origin :label "string")
                 :stream (make-string-input-stream buffer)))

(defmethod source-input-close ((source-input source-input))
  (close (source-input-stream source-input)))

(defun call-with-source-input (origin function)
  (let ((source-input (source-input-open origin)))
    (unwind-protect
         (funcall function source-input)
      (source-input-close source-input))))

(defmacro with-source-input (origin-var origin) &body
  `(call-with-source-input origin (lambda (,origin-var) ,@body)))
