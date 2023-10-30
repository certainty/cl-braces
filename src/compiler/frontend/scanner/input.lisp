(in-package :cl-braces.compiler.frontend.scanner)

(defclass source-location ()
  ((line :reader location-line :initarg :line :initform (error "no line given") :type (integer 1 *) :documentation "The line in the input stream, 1 based.")
   (column :reader location-column :initarg :column :initform (error "no column given") :type (integer 1 *) :documentation "The column in the input stream, 1 based.")
   (offset :reader location-offset :initarg :offset :initform (error "no offset given") :type (integer 0 *) :documentation "The offset in the input stream, 0 based."))
  (:documentation "A source location is a position in the input stream. It is used to denote the position of a token in the input stream"))

(defmethod print-object ((location source-location) stream)
  (with-slots (line column offset) location
    (print-unreadable-object (location stream :type t :identity t)
      (format stream "line: ~a column: ~a offset: ~a" line column offset))))

(defclass source-input ()
  ((uri :reader source-input-uri :initarg :uri :initform (error "no uri given") :type string :documentation "The uri of the input stream. This is used to identify the input stream.")
   (stream :reader source-input-stream :initarg :stream :initform (error "no stream given") :type stream :documentation "The input stream itself. This is used to read the input stream."))
  (:documentation "A source input is a stream of characters. It is used to read the input stream."))

(defmethod print-object ((input source-input) stream)
  (with-slots (uri) input
    (print-unreadable-object (input stream :type t :identity t)
      (format stream "uri: ~a" uri))))

(defclass string-input (source-input)
  ((label :reader :string-input-label :initarg :label :initform "anonymous" :type string :documentation "The label of the string input. This is used to identify the string input.")
   (string :reader :string-input-string :initarg :string :initform (error "no string given") :type string :documentation "The string that is being read."))
  (:documentation "A string input is a source input that reads from a string."))

(defmethod print-object ((input string-input) stream)
  (with-slots (label string) input
    (print-unreadable-object (input stream :type t :identity t)
      (format stream "label: ~a string: ~a" label string))))

(defmethod initialize-instance :around ((input string-input) &rest initargs &key &allow-other-keys)
  (let* ((string (getf initargs :string))
         (label (getf initargs :label))
         (uri (format nil "string://~a" label)))
    (call-next-method input :uri uri :label label :string string :stream (make-string-input-stream string))))

(defgeneric open-input (input-desginator &rest args))

(defgeneric close-input (source-input))

(defmethod open-input ((input-designator string) &rest args)
  (let ((label (getf args :label)))
    (make-instance 'string-input :string input-designator :label label)))

(defmethod close-input ((input string-input))
  nil)

(defun call-with-input (input-designator function &rest open-args)
  "Calls the given function with the input stream of the input designator. The input designator can be either a string"
  (let ((input (apply #'open-input input-designator open-args)))
    (unwind-protect
         (funcall function input)
      (close-input input))))

(defmacro with-input ((input-var input-designator &rest args &key &allow-other-keys) &body body)
  "Calls the given body with the input stream of the input designator."
  `(apply #'call-with-input ,input-designator (lambda (,input-var) ,@body) ,@args))
