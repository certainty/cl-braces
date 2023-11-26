(in-package :cl-braces.sourcecode.span)

(defclass source-span ()
  ((from
    :reader span-from
    :initarg :from
    :initform (error "must provide from")
    :type location:source-location
    :documentation "This is the location of the the first token or subexpression of the expression.")
   (to
    :reader span-to
    :initarg :to
    :initform (error "must provide to")
    :type location:source-location
    :documentation "This is the location of the last token or subexpression of the expression."))
  (:documentation "A span in the source code for a given syntatic element. It denotes a range from the begining of the element to the end of the element."))

(defmethod print-object ((span source-span) stream)
  (with-slots (from to) span
    (print-unreadable-object (span stream :type t :identity nil)
      (format stream "[~A, ~A]" from to))))

(defgeneric span-for (expression)
  (:documentation "Computes the span of the expression in the source code."))

(defun make-span (from to)
  (make-instance 'source-span :from from :to to))
