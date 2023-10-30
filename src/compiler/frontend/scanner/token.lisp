(in-package :cl-braces.compiler.frontend.scanner)

(deftype tpe-token-type ()
  "The type of a token. This is a simple enumeration of all the different kinds of tokens that can be encountered in the"
  '(member
    :tok-illegal
    :tok-eof))

(defclass token ()
  ((type :reader token-type :initarg :type :initform (error "no type given") :type tpe-token-type)
   (text :reader token-text :initarg :text :initform (error "no text given") :type string)
   (value :reader token-value :initarg :value :initform nil :type (or null t))
   (location :reader token-location :initarg :location :initform (error "no location given") :type source-location))
  (:documentation "A token is a single unit of input. It is the smallest unit of input that the parser can work with."))

(defmethod print-object ((token token) stream)
  (with-slots (type text value location) token
    (print-unreadable-object (token stream :type t :identity t)
      (format stream "type: ~a text: ~a value: ~a location: ~a" type text value location))))
