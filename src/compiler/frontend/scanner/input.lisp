(in-package :cl-braces.compiler.frontend.scanner)

(defclass source-input ()
  ((uri :reader source-input-uri
        :initarg :uri
        :initform (error "no uri given")
        :type string
        :documentation "An URI for the input. It is not required that you can construct an input from that URI. It's used mostly in program output.")
   (stream :reader source-input-stream
           :initarg :stream
           :initform (error "no stream given")
           :type stream
           :documentation "The input stream which must be a stream of characters."))
  (:documentation "A source-input is a combination of information where the data originates from and a stream that can be used to read from that data."))

(defmethod print-object ((input source-input) stream)
  (with-slots (uri) input
    (print-unreadable-object (input stream :type t :identity t)
      (format stream "uri: ~a" uri))))

(defclass string-input (source-input)
  ((label :initarg :label
          :initform "anonymous"
          :type string
          :documentation "The label of the string input. This is used to identify the string input.")
   (string :reader
           :string-input-string
           :initarg :string
           :initform (error "no string given")
           :type string
           :documentation "The string that is being read."))
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

(defgeneric open-input (input-desginator)
  (:documentation
   "Opens an input stream for the given input designator.
The call-site has to make sure that the input is properly closed again.
For most use-cases you should use `call-with-input' or `with-input' respectively, which take care of properly closing the input again. "))

(defgeneric close-input (source-input)
  (:documentation  "Closes the given input stream."))

(defmethod open-input ((inp source-input)) ; specialization for when we already have ac constructed input
  inp)

(defmethod open-input ((input-designator string))
  (make-instance 'string-input :string input-designator))

(defmethod close-input ((input string-input)) nil)

(defun call-with-input (input-designator fn)
  "Constructs a `source-input' from the given input designator and calls the provided `function' with that input.
The `input' is closed automatically after the `function' has been called."

  (let ((input (open-input input-designator)))
    (unwind-protect
         (funcall fn input)
      (close-input input))))
