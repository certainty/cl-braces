(in-package :cl-braces.sourcecode.location)

(defclass source-location ()
  ((line
    :reader line
    :initarg :line
    :initform (error "no line given")
    :type (integer 1 *)
    :documentation "The line in the input stream, 1 based.")
   (column
    :reader column
    :initarg :column
    :initform (error "no column given")
    :type (integer 1 *)
    :documentation "The column in the input stream, 1 based.")
   (offset
    :reader offset
    :initarg :offset
    :initform (error "no offset given")
    :type (integer 0 *)
    :documentation "The offset in the input stream, 0 based."))
  (:documentation "A source location is a position in the input stream. It is used to denote the position of a token in the input stream"))

(defmethod print-object ((location source-location) stream)
  (with-slots (line column offset) location
    (print-unreadable-object (location stream :type t :identity t)
      (format stream "line: ~a column: ~a offset: ~a" line column offset))))

(defmethod support:to-plist ((location source-location))
  (with-slots (line column offset) location
    (list :line line :column column :offset offset)))

(defun make-source-location (line column offset)
  "Creates a new source location from the given `line' and `column' and `offset'."
  (make-instance 'source-location :line line :column column :offset offset))

(defun line++ (location)
  (with-slots (line column offset) location
    (incf line)
    (setf column 1)
    (incf offset)))

(defun column++ (location)
  (with-slots (column offset) location
    (incf column)
    (incf offset)))

(defmethod support:copy-instance ((location source-location))
  (with-slots (line column offset) location
    (make-instance 'source-location :line line :column column :offset offset)))
