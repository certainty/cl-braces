(in-package :cl-braces.compiler.frontend.token)

(defclass location ()
  ((line :reader location-line
         :initarg :line
         :initform (error "no line given")
         :type (integer 1 *)
         :documentation "The line in the input stream, 1 based.")
   (column :reader location-column
           :initarg :column
           :initform (error "no column given")
           :type (integer 1 *)
           :documentation "The column in the input stream, 1 based.")
   (offset :reader location-offset
           :initarg :offset
           :initform (error "no offset given")
           :type (integer 0 *)
           :documentation "The offset in the input stream, 0 based."))
  (:documentation "A source location is a position in the input stream. It is used to denote the position of a token in the input stream"))

(defmethod print-object ((location location) stream)
  (with-slots (line column offset) location
    (print-unreadable-object (location stream :type t :identity t)
      (format stream "line: ~a column: ~a offset: ~a" line column offset))))

(deftype token-class ()
  "The token's ^class is union type of all possible token classes."
  '(member
    :@ILLEGAL
    :@EOF
    :@INTEGER))

(defclass token ()
  ((class :reader token-class
          :initarg :class
          :initform (error "no type given")
          :type token-class)
   (lexeme :reader token-lexeme
           :initarg :lexeme
           :initform (error "no lexeme given")
           :type string)
   (value :reader token-value
          :initarg :value
          :initform nil
          :type (or null t))
   (location :reader token-location
             :initarg :location
             :initform (error "no location given")
             :type location))
  (:documentation "A token is a single unit of input. It is the smallest unit of input that the parser can work with."))

(defmethod print-object ((token token) stream)
  (with-slots (type lexeme value location) token
    (print-unreadable-object (token stream :type t :identity t)
      (format stream "type: ~a lexeme: ~a value: ~a location: ~a" type lexeme value location))))

(-> class= (token token-class) boolean)
(defun class= (token expected-class)
  "Returns true if the token's class is equal to the given `expected-class.'"
  (eql (token-class token) expected-class))
