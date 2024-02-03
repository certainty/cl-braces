(in-package :cl-braces.sourcecode.span)

(defclass source-span ()
  ((from
    :reader from
    :initarg :from
    :initform (error "must provide from")
    :type location:source-location
    :documentation "This is the location of the the first token or subexpression of the expression.")
   (to
    :reader to
    :initarg :to
    :initform (error "must provide to")
    :type location:source-location
    :documentation "This is the location of the last token or subexpression of the expression."))
  (:documentation "A span in the source code for a given syntatic element. It denotes a range from the begining of the element to the end of the element."))

(defmethod print-object ((span source-span) stream)
  (with-slots (from to) span
    (print-unreadable-object (span stream :type t :identity nil)
      (format stream "[~A, ~A]" from to))))

(defun make-span (from to)
  (make-instance 'source-span :from from :to to))

(defparameter *dummy-location* (location:make-source-location 0 0 0))
(defparameter *dummy-span* (make-span *dummy-location* *dummy-location*))

(defmethod support:to-plist ((span source-span))
  (with-slots (from to) span
    (list :from (support:to-plist from) :to (support:to-plist to))))

(defgeneric for (obj)
  (:documentation "Computes the span of the expression in the source code."))

(defmethod for (obj)
  (declare (ignore obj))
  *dummy-span*)

(defmethod support:copy-instance ((span source-span))
  (make-instance 'source-span :from (from span) :to (to span)))
