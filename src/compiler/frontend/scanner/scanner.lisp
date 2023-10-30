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

(defparameter *fail-fast* nil "If true, the scanner will enter the debugger when an error is encountered")

(define-condition scan-error (error)
  ((message :initarg :message :reader scan-error-message)
   (location :initarg :location :reader scan-error-location))
  (:report (lambda (condition stream)
             (format stream "Illegal token at ~a" (scan-error-location condition)))))

(defclass state ()
  ((input :reader state-input :initarg :input :initform (error "no input given") :type source-input :documentation "The input that is being scanned. Must be a character stream.")
   (input-stream :initform nil :type (or null stream) :documentation "The input stream that is being scanned. Must be a character stream.")

   (consumed :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer 0) :type (vector character) :documentation "The characters that have been consumed so far")
   (token-offset  :initarg :start :initform 0 :type integer :documentation "The start position of the current token")
   (token-column :initarg :column :initform 1 :type integer :documentation "The column in the input stream, where the current token starts")
   (token-line  :initarg :line :initform 1 :type integer :documentation "The line in the input stream, where the current token starts")

   (stream-line  :initarg :line :initform 1 :type integer :documentation "The line in the input stream, where the current token starts")
   (stream-column :initarg :column :initform 1 :type integer :documentation "The column in the input stream, where the current token starts")
   (stream-offset :initarg :offset :initform 0 :type integer :documentation "The offset in the input stream, where the current token starts"))
  (:documentation "The state of the scanner. This is the object that is passed around to all the different functions that scan the input"))

(defmethod print-object ((state state) stream)
  (with-slots (input consumed token-offset stream-line stream-column stream-offset) state
    (print-unreadable-object (state stream :type t :identity t)
      (format stream "input: ~a consumed: ~a start: ~a line: ~a column: ~a offset: ~a" input consumed token-offset stream-line stream-column stream-offset))))

(defmethod initialize-instance :after ((state state) &key)
  (with-slots (input-stream input) state
    (setf input-stream (source-input-stream input))))

(defun call-with-scanner (input-designator function &rest args)
  "Calls the given function with a new scanner that is initialized with the input stream of the input designator."
  (call-with-input input-designator
                   (lambda (input)
                     (let ((state (make-instance 'state :input input)))
                       (apply function state args)))))

(defun open-scanner (input-designator &rest args)
  "Opens a new scanner that is initialized with the input stream of the input designator."
  (make-instance 'state :input (open-input input-designator args)))

(defmacro with-scanner ((state-var input-designator &rest args) &body body)
  "Calls the given body with a new scanner that is initialized with the input stream of the input designator."
  `(apply #'call-with-scanner
    ,input-designator (lambda (,state-var) ,@body)
    ,@args))

(-> next-token (state) token)
(defun next-token (state)
  "Reads the next available token from the input stream. Unless something catastrophic happens this function will always
return a token. There are special kinds of tokens that denote `illegal input' as well as `end of input'"
  (handler-bind ((scan-error (lambda (e)
                               (if *fail-fast*
                                   (invoke-debugger e)
                                   (when (find-restart 'continue)
                                     (invoke-restart 'continue))))))
    (%next-token state)))

(-> %next-token (state) token)
(defun %next-token (state)
  (with-slots (token-offset token-column token-line stream-offset stream-column stream-line) state
    (setf token-offset stream-offset)
    (setf token-column stream-column)
    (setf token-line stream-line)
    (or
     (scan-eof state)
     (scan-illegal state))))

(-> scan-eof (state) (or null token))
(defun scan-eof (state)
  (when (eofp state)
    (accept state :tok-eof)))

(-> scan-illegal (state)  token)
(defun scan-illegal (state)
  (advance! state)
  (accept state :tok-illegal))

(-> eofp (state) boolean)
(defun eofp (state)
  "Returns true if the scanner has reached the end of the input stream."
  (null (peek state)))

(-> peek (state) (or null character))
(defun peek (state)
  "Peeks at the next character in the input stream without advancing the scanner."
  (with-slots (input-stream) state
    (peek-char nil input-stream nil nil)))

(-> advance! (state) (or null character))
(defun advance! (scanner)
  "Advance the scanner to the next character, adjusting internal state to keep track of location in the input stream.
   Returns the character that was advanced to or nil if the end of the input has been reached."
  (with-slots (input-stream stream-line stream-offset stream-column consumed) scanner
    (let ((current-char (read-char input-stream nil)))
      (when current-char
        (incf stream-column)
        (incf stream-offset)
        (vector-push-extend current-char consumed)
        (when (eql current-char #\Newline)
          (incf stream-line)
          (setf stream-column 0))
        current-char))))

(-> accept (state tpe-token-type &key (to-value t)) token)
(defun accept (scanner token-type &key (to-value #'identity))
  "Accept the next token and return it. It the token is illegal the scate will signal a contiuabl scan-error."
  (with-slots (consumed token-offset token-column token-line) scanner
    (let* ((token-text (coerce consumed 'string))
           (value (funcall to-value token-text))
           (location (make-instance 'source-location :line token-line :column token-column :offset token-offset)))
      (setf consumed (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
      (let ((token (make-instance 'token :type token-type :text token-text :value value :location location)))
        (prog1 token
          (when (eql token-type :tok-illegal)
            (cerror "Illegal token" (make-condition 'scan-error :message "Illegal token" :location location))))))))
