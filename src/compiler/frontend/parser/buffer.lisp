(in-package :cl-braces.compiler.frontend.parser)

(defclass parse-buffer ()
  ((buffer
    :initarg :buffer
    :initform (error "must provide array of tokens")
    :type (vector token:token *)
    :documentation "The buffer of tokens that is used by the parser to parse the input stream.")
   (save-points
    :initform nil
    :type list
    :documentation "A stack of save points that is used to backtrack to a previous position in the input stream.")
   (cursor
    :initform 0
    :type a:array-index))
  (:documentation "A parse buffer is a buffer of tokens that is used by the parser to parse the input stream. It allows to look ahead in the input stream and to backtrack to a previous position in the input stream."))

(defmethod print-object ((parse-buffer parse-buffer) stream)
  (with-slots (buffer cursor save-points) parse-buffer
    (print-unreadable-object (parse-buffer stream :type t)
      ;; get an exercpt of the buffer surrounding the current cursor position
      (let ((excerpt (subseq buffer (max 0 (- cursor 5)) (min (length buffer) (+ cursor 5)))))
        (format stream "cursor: ~a save-points: ~a tokens: ~%" cursor save-points)
        (loop for i from 0 below (length excerpt)
              for token = (aref excerpt i)
              do (format stream "~a ~a~%" (token:class token) (if (= i cursor) "<*>" "")))))))

(defun make-parse-buffer (tokens)
  (make-instance 'parse-buffer :buffer tokens))

(-> read-token (parse-buffer) (or null token:token))
(defun read-token (parse-buffer)
  (with-slots (cursor buffer) parse-buffer
    (when (< cursor (length buffer))
      (prog1 (aref buffer cursor)
        (incf cursor)))))

(-> save-point (parse-buffer) a:array-index)
(defun save-point (parse-buffer)
  (with-slots (cursor save-points) parse-buffer
    (push cursor save-points)
    cursor))

(-> rollback-to-save-point (parse-buffer) (or null  a:array-index))
(defun rollback-to-save-point (parse-buffer)
  (with-slots (cursor save-points) parse-buffer
    (when save-points
      (setf cursor (pop save-points))
      cursor)))

(-> commit-save-point (parse-buffer) (or null a:array-index))
(defun commit-save-point (parse-buffer)
  (with-slots (cursor save-points) parse-buffer
    (when save-points
      (pop save-points))))
