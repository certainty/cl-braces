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

(-> make-parse-buffer (scanner:state) parse-buffer)
(defun make-parse-buffer (scanner)
  ;; collect all tokens into an array
  (let ((buffer (make-array 10  :fill-pointer 0 :adjustable t :element-type 'token:token :initial-element (token:synthetic-eof))))
    (loop for token = (scanner:next-token scanner)
          if (token:class= token token:@EOF)
            do
               (progn
                 (vector-push-extend token buffer)
                 (return (make-instance 'parse-buffer :buffer buffer)))
          else
            do (vector-push-extend token buffer))))

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
