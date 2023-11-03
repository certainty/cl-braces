(in-package :cl-braces.compiler.frontend.scanner)

;;;; The scanner is the first stage of the compiler. It takes a stream of characters and produces a stream of tokens.
;;;; A token is a representation of a lexeme in the input stream bundled with some metadata about the location and the class of the lexeme.
;;;; The token's class is used by the parser to determine how to interpret the token.
;;;;
;;;; The scanner is implemented as a state machine. The state is represented by the `state' class. The state is used to keep track of the input stream
;;;; and the current lexeme that's being scanned. The state is passed around to all the functions that implement the state machine.
;;;;
;;;; The main API to the scanner are the following functions:
;;;;
;;;; * `call-with-scanner' - Calls the given function with a new scanner that is initialized with the input stream of the input designator.
;;;; * `next-token' - Reads the next available token from the input stream.
;;;;
;;;; Usage:
;;;;
;;;; ```common-lisp
;;;; (call-with-scanner "3 * (3-5)"
;;;;   (lambda (state)
;;;;    (let ((all-tokens (loop for token = (next-token state) until (eofp state) collect token)))
;;;;      (format t "~a~%" all-tokens))))
;;;; ```

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
                  :initform 1
                  :type integer
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

(defun open-scanner (input-designator)
  "Opens a new scanner that is initialized with the input stream of the input designator."
  (make-instance 'state :input (open-input input-designator)))

(-> next-token (state) token:token)
(defun next-token (state)
  "Reads the next available token from the input stream. Unless something catastrophic happens this function will always
return a token. There are two special token classes which are used to denote eof and illegal tokens respectively.

Those are:
 - `@ILLEGAL' for when no token could be recognized
 - `@EOF' for when the end of stream has been reached

You should use the `class=' predicate to test for these.
The following example shows how to deal with these cases:

```common-lisp
(scanner:with-scanner (s \"3 +3\")
  (loop
    (let ((token (scanner:next-token s)))
      (cond
        ((token:class= token token:@ILLEGAL) (print \"Illegal token\"))
        ((token:class= token token:@EOF) (return))
        (otherwise (print token))))))
```
"
  (handler-bind ((scan-error (lambda (e)
                               (if *fail-fast*
                                   (invoke-debugger e)
                                   (when (find-restart 'continue)
                                     (invoke-restart 'continue))))))
    (%next-token state)))

(-> %next-token (state) token:token)
(defun %next-token (state)
  (skip-whitespaces! state)

  (with-slots (token-offset token-column token-line stream-offset stream-column stream-line lexeme) state
    (setf lexeme (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
    (setf token-offset stream-offset)
    (setf token-column stream-column)
    (setf token-line stream-line)

    (or
     (scan-eof state)
     (scan-integer state)
     (scan-one-char-tokens state)
     (scan-illegal state))))

(-> scan-integer (state) (or null token:token))
(defun scan-integer (state)
  (let  ((digit (peek state)))
    (when (digit-char-p digit)
      (advance! state)
      (scan-digits state)
      (accept state token:@INTEGER #'parse-integer))))

(-> scan-digits (state) (or null list))
(defun scan-digits (state)
  (scan-while state (lambda (c) (and c (digit-char-p c)))))

(-> scan-one-char-tokens (state) (or null token:token))
(defun scan-one-char-tokens (state)
  (let ((c (peek state)))
    (macrolet ((=> (token-class) `(progn (advance! state) (accept state ,token-class))))
      (case c
        (#\( (=> token:@LPAREN))
        (#\) (=> token:@RPAREN))
        (#\+ (=> token:@PLUS))
        (#\- (=> token:@MINUS))
        (#\/ (=> token:@SLASH))
        (#\* (=> token:@STAR))))))

(-> scan-eof (state) (or null token:token))
(defun scan-eof (state)
  (when (eofp state)
    (accept state token:@EOF)))

(-> scan-illegal (state)  token:token)
(defun scan-illegal (state)
  (advance! state)
  (accept state token:@ILLEGAL))

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

(serapeum:defconst +whitespace+ (list #\Space #\Tab #\Return #\Newline))

(defun skip-whitespaces! (state)
  "Skip whitespaces and comments"
  (loop for (c1 c2) = (multiple-value-list (peek2 state)) until (null c1) do
    (cond
      ((member c1 +whitespace+) (advance! state :skip-p t))
      ((and (eql #\/ c1) (eql #\/ c2))
       (advance! state :skip-p t)
       (advance! state :skip-p t)
       (loop for n = (advance! state :skip-p t) until (or (eql #\Newline n) (eofp state))))
      (t (return)))))

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

(-> advance! (state &key (:skip-p boolean)) (or null character))
(defun advance! (scanner &key (skip-p nil))
  "Advance the scanner to the next character, adjusting internal state to keep track of location in the input stream.
   Returns the character that was advanced to or nil if the end of the input has been reached."
  (with-slots (input-stream stream-line stream-offset stream-column lexeme token-offset) scanner
    (let ((current-char (read-char input-stream nil)))
      (when current-char
        (incf stream-column)
        (incf stream-offset)
        (if skip-p
            (setf token-offset stream-offset)
            (vector-push-extend current-char lexeme))
        (when (eql current-char #\Newline)
          (incf stream-line)
          (setf stream-column 1))
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
If `token-class' is token:@ILLEGAL then a `scan-error' condition is signalled."
  (with-slots (lexeme token-offset token-column token-line) scanner
    (let* ((token-lexeme (coerce lexeme 'string))
           (value (funcall to-value token-lexeme))
           (location (make-instance 'token:source-location :line token-line :column token-column :offset token-offset)))
      (let ((token (make-instance 'token:token :class token-class :lexeme lexeme :value value :location location)))
        (prog1 token
          (when (token:class= token token:@ILLEGAL)
            (cerror "Illegal token" (make-condition 'scan-error :message "Illegal token" :location location))))))))
