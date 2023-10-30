(in-package :cl-braces.compiler.frontend.scanner)

(defparameter *fail-fast* nil "If true, the scanner will enter the debugger when an error is encountered")

(define-condition scan-error (error)
  ((message :initarg :message :reader scan-error-message)
   (location :initarg :location :reader scan-error-location))
  (:report (lambda (condition stream)
             (format stream "Illegal token at ~a" (scan-error-location condition)))))

(defclass state ()
  ((input :reader
          state-input
          :initarg :input
          :initform (error "no input given")
          :type source-input
          :documentation "The input that the scanner reads from.")

   (input-stream :initform nil
                 :type (or null stream)
                 :documentation "The input stream as retrieved from the input. This slot is used to cache the stream.")

   (lexeme :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
           :type (vector character)
           :documentation "The lexeme consumed thus far. This is pure internal state for the duration of an in-progress scan.")

   (token-offset  :initarg :token-offset
                  :initform 0
                  :type integer
                  :documentation "The offset in the input stream of the lexeme that's currently being scanned")

   (token-column :initarg
                 :token-column
                 :initform 1
                 :type integer
                 :documentation "The column in the input stream where the currently scanned lexeme started")

   (token-line  :initarg :token-line
                :initform 1
                :type integer
                :documentation "The line in the input stream where the currently scanned lexeme started")

   (stream-offset :initarg :stream-offset
                  :initform 0
                  :type integer
                  :documentation "The offset in the input stream that the scanner is currently at")

   (stream-line  :initarg :stream-line
                 :initform 1
                 :type integer
                 :documentation "The line in the input stream that the scanner is currently at")

   (stream-column :initarg
                  :stream-column
                  :initform 1 :type integer
                  :documentation "The column in the input stream that the scanner is currently at"))
  (:documentation "The state that's used to keep track during a scan of the input stream."))

(defmethod print-object ((state state) stream)
  (with-slots (lexeme token-offset token-line token-column stream-offset) state
    (print-unreadable-object (state stream :type t :identity t)
      (format stream "lexeme: ~a token-offset: ~a token-line: ~a token-column: ~a stream-offset: ~a" lexeme token-offset token-line token-column stream-offset))))

(defmethod initialize-instance :after ((state state) &key)
  (with-slots (input-stream input) state
    (setf input-stream (source-input-stream input))))

(defun call-with-scanner (input-designator function &rest args)
  "Calls the given function with a new scanner that is initialized with the input stream of the input designator.
The scanner makes sure that the input stream is closed correctly on error or when the scan has finished.
It uses `call-with-input' which inturn ensures that.
"
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

(-> next-token (state) token:token)
(defun next-token (state)
  "Reads the next available token from the input stream. Unless something catastrophic happens this function will always
return a token. There are special kinds of tokens that denote `illegal input' as well as `end of input'"
  (handler-bind ((scan-error (lambda (e)
                               (if *fail-fast*
                                   (invoke-debugger e)
                                   (when (find-restart 'continue)
                                     (invoke-restart 'continue))))))
    (%next-token state)))

(-> %next-token (state) token:token)
(defun %next-token (state)
  (with-slots (token-offset token-column token-line stream-offset stream-column stream-line lexeme) state
    (setf lexeme (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
    (setf token-offset stream-offset)
    (setf token-column stream-column)
    (setf token-line stream-line)

    (or
     (scan-eof state)
     (scan-integer state)
     (scan-illegal state))))

(-> scan-integer (state) (or null token:token))
(defun scan-integer (state)
  (multiple-value-bind (sign digit) (peek2 state)
    (cond
      ((and (or (eql #\+ sign) (eql #\- sign)) (digit-char-p digit))
       (advance! state)
       (scan-digits state)
       (accept state :@INTEGER #'parse-integer))
      ((scan-digits state)
       (accept state :@INTEGER #'parse-integer)))))


(-> scan-digits (state) (or null list))
(defun scan-digits (state)
  (scan-while state (lambda (c) (and c (digit-char-p c)))))

(-> scan-eof (state) (or null token:token))
(defun scan-eof (state)
  (when (eofp state)
    (accept state :@EOF)))

(-> scan-illegal (state)  token:token)
(defun scan-illegal (state)
  (advance! state)
  (accept state :@ILLEGAL))

(-> scan-while (state t) (or null list))
(defun scan-while (state predicate)
  "Consume the input, returning the list of consumed characters, while `predicate' returns t"
  (loop for next = (advance-when! state predicate)
        until (null next)
        collect next))

(-> eofp (state) boolean)
(defun eofp (state)
  "Returns true if the scanner has reached the end of the input stream."
  (null (peek state)))

(-> peek (state) (or null character))
(defun peek (state)
  "Peeks at the next character in the input stream without advancing the scanner."
  (with-slots (input-stream) state
    (peek-char nil input-stream nil nil)))

(-> peek2 (state) (values (or null character) (or null character)))
(defun peek2 (state)
  "Peeks two characters ahead in the input stream without advancing the scanner."
  (with-slots (input-stream) state
    (let* ((c1 (read-char input-stream nil))
           (c2 (peek-char nil input-stream nil nil)))
      (when c1 (unread-char c1 input-stream))
      (values c1 c2))))

(-> advance! (state) (or null character))
(defun advance! (scanner)
  "Advance the scanner to the next character, adjusting internal state to keep track of location in the input stream.
   Returns the character that was advanced to or nil if the end of the input has been reached."
  (with-slots (input-stream stream-line stream-offset stream-column lexeme) scanner
    (let ((current-char (read-char input-stream nil)))
      (when current-char
        (incf stream-column)
        (incf stream-offset)
        (vector-push-extend current-char lexeme)
        (when (eql current-char #\Newline)
          (incf stream-line)
          (setf stream-column 0))
        current-char))))

(-> advance-when! (state t) (or null character))
(defun advance-when! (state predicate)
  "Advance in the stream if `predicate' returns true when applied to the next character in stream"
  (when (funcall predicate (peek state))
    (advance! state)))

(-> accept (state token:token-class &optional function) token:token)
(defun accept (scanner token-class &optional (to-value #'identity))
  "Accept the scanned lexeme and constructs a `token:token' from it using the provided `token-class'.
If `to-value' is provided it must be a function of one argument that will be applied to the lexeme to produce the value of the token.
If `token-class' is :@ILLEGAL then a `scan-error' condition is signalled."
  (with-slots (lexeme token-offset token-column token-line) scanner
    (let* ((token-lexeme (coerce lexeme 'string))
           (value (funcall to-value token-lexeme))
           (location (make-instance 'source-location :line token-line :column token-column :offset token-offset)))
      (let ((token (make-instance 'token :class token-class :lexeme lexeme :value value :location location)))
        (prog1 token
          (when (token:class= token :@ILLEGAL)
            (cerror "Illegal token" (make-condition 'scan-error :message "Illegal token" :location location))))))))