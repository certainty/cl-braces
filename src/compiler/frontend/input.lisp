(in-package :cl-braces/compiler/frontend)

(defclass source-origin () ()
  (:documentation "An origin for source code"))

(defgeneric source-uri (origin)
  (:documentation "Return the URI of the source origin."))

(defclass file-origin (source-origin)
  ((path :initarg :path :reader path)))

(defmethod source-uri ((origin file-origin))
  (format nil "file://~A" (path origin)))

(defclass string-origin (source-origin)
  ((label :initarg :label :reader label)))

(defmethod source-uri ((origin string-origin))
  (format nil "string://~A" (label origin)))

(defstruct source-input
  (origin nil :type (or null source-origin))
  (buffer "" :type (or null string)))

(defgeneric create-source-input (origin)
  (:documentation "Create a source input from a source origin."))

(defmethod create-source-input ((origin pathname))
  (with-open-file (stream origin)
    (make-source-input :origin (make-instance 'file-orgin :path origin)
                       :buffer (read-file-to-string origin))))

(defmethod create-source-input ((origin string))
  (make-source-input :origin (make-instance 'string-origin :label "string")
                     :buffer origin))

(defun read-file-to-string (filename)
  (with-open-file (stream filename :element-type 'character)
    (let* ((length (file-length stream))
           (buffer (make-string length)))
      (read-sequence buffer stream)
      buffer)))
