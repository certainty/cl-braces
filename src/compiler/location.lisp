(in-package :cl-braces.compiler.location)

(defclass source-location ()
  ((line :reader line
         :initarg :line
         :initform (error "no line given")
         :type (integer 1 *)
         :documentation "The line in the input stream, 1 based.")
   (column :reader column
           :initarg :column
           :initform (error "no column given")
           :type (integer 1 *)
           :documentation "The column in the input stream, 1 based.")
   (offset :reader offset
           :initarg :offset
           :initform (error "no offset given")
           :type (integer 0 *)
           :documentation "The offset in the input stream, 0 based."))
  (:documentation "A source location is a position in the input stream. It is used to denote the position of a token in the input stream"))

(defmethod print-object ((location source-location) stream)
  (with-slots (line column offset) location
    (print-unreadable-object (location stream :type t :identity t)
      (format stream "line: ~a column: ~a offset: ~a" line column offset))))

(defclass source-span ()
  ((from :reader span-from
         :initarg :from
         :initform (error "must provide from")
         :type source-location
         :documentation "This is the location of the the first token or subexpression of the expression."
         )
   (to :reader span-to
       :initarg :to
       :initform (error "must provide to")
       :type source-location
       :documentation "This is the location of the last token or subexpression of the expression."))
  (:documentation "A span in the source code for a given syntatic element. It denotes a range from the begining of the element to the end of the element."))

(defmethod print-object ((span source-span) stream)
  (with-slots (from to) span
    (print-unreadable-object (span stream :type t :identity t)
      (format stream "[~A, ~A]" from to))))

(defgeneric span-for (expression)
  (:documentation "Computes the span of the expression in the source code."))
