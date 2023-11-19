(in-package :cl-braces.compiler.frontend.scanner)
;;;; The scanner is the first stage of the compiler. It takes a stream of characters and produces a stream of tokens.
;;;; A token is a representation of a lexeme in the input stream bundled with some metadata about the location and the class of the lexeme.
;;;; The token's class is used by the parser to determine how to interpret the token.
;;;;
;;;; The scanner is implemented as a state machine. The state is represented by the `state' class. The state is used to keep track of the input stream
;;;; and the current lexeme that's being scanned. The state is passed around to all the functions that implement the state machine.
;;;;
;;;; ## Main API
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
;;;;
;;;; ## Lexical elements from the language specification
;;;;
;;;; The scanner follows the [golang language specification for lexical elements](https://golang.org/ref/spec#Lexical_elements).
;;;; I note explicitely any deviations from that specification. These could be required to make the language easier.
;;;;
;;;; ## Organisation of the source code
;;;;
;;;; In order to allow easy navigation the source-code is devided into the followin main sections:
;;;;
;;;; - The necessary objects and utilities to hold the state of the scanner and represent errors.
;;;; - The main API functions
;;;; - Functions that deal with whitespace, comments and [insertion of semicolons](https://golang.org/ref/spec#Semicolons)
;;;; - Functions that implement the scanner for each of the four classes of tokens: identifiers, keywords, operators and punctuation, and literals.
;;;; - Utility functions that are used by the scanner functions


;;; =============================================================
;;; Objects required to represent the scanner state and errors
;;; =============================================================

(define-condition scan-error (error)
  ((message
    :reader scan-error-message
    :initarg :message )
   (location
    :reader scan-error-location
    :initarg :location ))
  (:report (lambda (condition stream)
             (format stream "Illegal token at ~a" (scan-error-location condition)))))

(defclass state ()
  ((input
    :reader state-input
    :initarg :input
    :initform (error "no input given")
    :type source-input
    :documentation "The input that the scanner reads from.")

   (input-stream
    :initform nil
    :type (or null stream)
    :documentation "The input stream as retrieved from the input. This slot is used to cache the stream.")

   (lexeme
    :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
    :type (vector character)
    :documentation "The lexeme consumed thus far. This is pure internal state for the duration of an in-progress scan.")

   (last-token
    :initform nil
    :type (or null token:token)
    :documentation "The preview token that was scanned. This is required to correctly inject semicolons when necessary.")

   (fail-fast
    :initarg :fail-fast
    :initform nil
    :type boolean
    :documentation "If true then the scanner will signal a `scan-error' condition when it encounters an illegal token.
                     If false then the scanner will try to recover from an illegal token by skipping over it and continuing to scan.")

   (token-offset
    :initarg :token-offset
    :initform 0
    :type integer
    :documentation "The offset in the input stream of the lexeme that's currently being scanned")

   (token-column
    :initarg
    :token-column
    :initform 1
    :type integer
    :documentation "The column in the input stream where the currently scanned lexeme started")

   (token-line
    :initarg :token-line
    :initform 1
    :type integer
    :documentation "The line in the input stream where the currently scanned lexeme started")

   (stream-offset
    :initarg :stream-offset
    :initform 0
    :type integer
    :documentation "The offset in the input stream that the scanner is currently at")

   (stream-line
    :initarg :stream-line
    :initform 1
    :type integer
    :documentation "The line in the input stream that the scanner is currently at")

   (stream-column
    :initarg
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

;;; ===================
;;; Main API functions
;;; ===================

(-> call-with-scanner ((function (state) *) input-designator &key (:fail-fast boolean))  *)
(defun call-with-scanner (function input-designator &key (fail-fast nil))
  "Calls the given `function' with a new `scanner' that is initialized with the input stream of the `input-designator'.
   If `fail-fast' is true then the scanner will signal a `scan-error' condition when it encounters an illegal token.
   The scanner makes sure that the input stream is closed correctly on error or when the scan has finished.
   It uses `call-with-input' which inturn ensures that.
  "
  (call-with-input
   (lambda (input)
     (let ((state (make-instance 'state :input input :fail-fast fail-fast)))
       (funcall function state)))
   input-designator))

(defun open-scanner (input-designator &key (fail-fast nil))
  "Opens a new scanner that is initialized with the input stream of the `input-designator'.
   If `fail-fast' is true then the scanner will signal a `scan-error' condition when it encounters an illegal token.
   Returns a fresh instance of `scanner'."
  (make-instance 'state :input (open-input input-designator) :fail-fast fail-fast))

(defun fail-fast! (state &optional (should-fail-fast t))
  "Sets the `fail-fast' slot of the `state' to `should-fail-fast'"
  (with-slots (fail-fast) state
    (setf fail-fast should-fail-fast)))

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
   ;; assuming you have a scanner created
   (loop
    (let ((token (scanner:next-token scanner)))
      (cond
        ((token:class= token token:@ILLEGAL) (print \"Illegal token\"))
        ((token:class= token token:@EOF) (return))
        (otherwise (print token))))))
   ```
  "
  (with-slots (fail-fast) state
    (handler-bind
        ((scan-error (lambda (e)
                       (if fail-fast
                           (invoke-debugger e)
                           (when (find-restart 'continue)
                             (invoke-restart 'continue))))))
      (%next-token state))))

(-> %next-token (state) token:token)
(defun %next-token (state)
  (with-slots (token-offset token-column token-line stream-offset stream-column stream-line lexeme) state
    (let ((maybe-semicolon (scan-inter-token-space state)))
      (setf lexeme (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
      (setf token-offset stream-offset)
      (setf token-column stream-column)
      (setf token-line stream-line)
      (or
       maybe-semicolon
       (scan-eof state)
       (scan-literal state)
       (scan-operator/punctuation state)
       (scan-identifier state)
       (scan-illegal state)))))

;;; =============================================================
;;; Functions that deal with whitespace, comments and semicolons
;;; =============================================================

(a:define-constant +whitespace+ (list #\Space #\Tab #\Return #\Newline) :test #'equalp)

(declaim (inline inter-token-space-p))
(defun inter-token-space-p (c)
  "Returns `t' if `c' is considered inter-token whitespace.
   - space (U+0020)
   - horizontal tab (U+0009)
   - carriage return (U+000D)
   - newlines (U+000A)"
  (member c +whitespace+))

(-> scan-inter-token-space (state) (or null token:token))
(defun scan-inter-token-space (state)
  "Scan inter-token whitespace, returning either nil in which case it just skipped over whitespace/comments or a token if it inserted a semicolon.
   The insertion of semicolons is done according to the [golang language specification](https://golang.org/ref/spec#Semicolons).
  "
  (loop for (c1 c2) = (multiple-value-list (peek2 state)) until (null c1) do
    (cond
      ((inject-semicolon-p state c1)
       (return-from scan-inter-token-space (accept state token:@SEMICOLON)))
      ((inter-token-space-p c1) (advance! state :skip-p t)) ; just skip over whitespace
      ((and (eql #\/ c1) (eql #\/ c2)) ; comments
       (advance! state :skip-p t)
       (advance! state :skip-p t)
       (loop for n = (advance! state :skip-p t) until (or (eql #\Newline n) (eofp state))))
      (t (return)))))

(defun inject-semicolon-p (state c1)
  (with-slots (last-token) state
    (when (and (eql #\Newline c1) last-token)
      (or
       (token:literal-p last-token)
       (token:identifier-p last-token)
       (token:class-any-p last-token token:@BREAK token:@CONTINUE token:@FALLTHROUGH token:@RETURN)
       (token:class-any-p last-token token:@PLUS_PLUS token:@MINUS_MINUS token:@RPAREN token:@RBRACKET token:@RBRACE)))))

;;; From the golang language specification: https://golang.org/ref/spec#Tokens
;;; > There are four classes: identifiers, keywords, operators and punctuation, and literals.
;;;
;;; The following functions implement the scanner for each of those classes.
;;; In order to keep navigation simple we'll also use the same order


;;; ===================================================
;;; Identifier / Keywords
;;;
;;; identifier = letter { letter | unicode_digit } .
;;;
;;; A keyword is an identifier with one of the following lexemes:
;;; break        default      func         interface    select
;;; case         defer        go           map          struct
;;; chan         else         goto         package      switch
;;; const        fallthrough  if           range        type
;;; continue     for          import       return       var
;;; ==================================================

(-> scan-identifier (state) (or null token:token))
(defun scan-identifier (state)
  "Scan an identifier which is either a user-defined identifier or a keyword."
  (with-slots (lexeme) state
    (a:when-let ((c (peek state)))
      (when (letter-p c)
        (advance! state)
        (scan-while state #'identifier-char-p)
        (let ((identifier (coerce lexeme 'string)))
          (a:if-let ((keyword (gethash identifier +keywords+)))
            (accept state keyword)
            (accept state token:@IDENTIFIER)))))))

(a:define-constant +keywords+
    ;; dictionary mapping keyword lexemes to their class
    (s:dict
     "if" token:@IF
     "else" token:@ELSE
     "break" token:@BREAK
     "continue" token:@CONTINUE
     "fallthrough" token:@FALLTHROUGH
     "return" token:@RETURN)
  :test #'equalp)

(declaim (inline identifier-char-p))
(defun identifier-char-p (c)
  (or (letter-p c) (unicode-digit-p c)))

(declaim (inline letter-p))
(defun letter-p (c)
  "Returns true if `c' is a letter according to the golang language specification"
  (and c (or (unicode-letter-p c) (eql c #\_))))

(declaim (inline unicode-letter-p))
(defun unicode-letter-p (c)
  "Returns true if `c' is a unicode letter according to the golang language specification"
  (and c (sb-unicode:alphabetic-p c)))

(defun unicode-digit-p (c)
  "Returns true if `c' is a unicode digit according to the golang language specification"
  (and c (digit-char-p c)))


;;; ===================================================
;;; Operators and punctuation
;;; ===================================================

(defun scan-operator/punctuation (state)
  (or (scan-two-char-tokens state)
      (scan-one-char-tokens state)))

(defun scan-two-char-tokens (state)
  (multiple-value-bind (c1 c2) (peek2 state)
    (macrolet ((=> (token-class) `(progn (advance! state) (advance! state) (accept state ,token-class))))
      (cond
        ((and (eql c1 #\:) (eql c2 #\=))
         (=> token:@COLON_EQUAL))
        ((and (eql c1 #\<) (eql c2 #\=))
         (=> token:@LE))
        ((and (eql c1 #\>) (eql c2 #\=))
         (=> token:@GE))
        ((and (eql c1 #\-) (eql c2 #\-))
         (=> token:@MINUS_MINUS))
        ((and (eql c1 #\+) (eql c2 #\+))
         (=> token:@PLUS_PLUS))))))

(defun scan-one-char-tokens (state)
  (let ((c (peek state)))
    (macrolet ((=> (token-class) `(progn (advance! state) (accept state ,token-class))))
      (case c
        (#\( (=> token:@LPAREN))
        (#\) (=> token:@RPAREN))
        (#\{ (=> token:@LBRACE))
        (#\} (=> token:@RBRACE))
        (#\[ (=> token:@LBRACKET))
        (#\] (=> token:@RBRACKET))
        (#\+ (=> token:@PLUS))
        (#\- (=> token:@MINUS))
        (#\/ (=> token:@SLASH))
        (#\* (=> token:@STAR))
        (#\; (=> token:@SEMICOLON))
        (#\, (=> token:@COMMA))
        (#\< (=> token:@LT))
        (#\> (=> token:@GT))))))

;;; ===================================================
;;; Literals
;;; ===================================================

(-> scan-literal (state) (or null token:token))
(defun scan-literal (state)
  (or (scan-integer state)))

(-> scan-integer (state) (or null token:token))
(defun scan-integer (state)
  "Scan an integer literal."
  (scan-decimal-literal state))

(defun scan-decimal-literal (state)
  (let ((first-digit (peek state)))
    (when (unicode-digit-p first-digit)
      (advance! state)
      (when (eql first-digit #\0)
        (return-from scan-decimal-literal (accept state token:@INTEGER #'parse-integer)))
      (scan-decimal-digits state)
      (accept state token:@INTEGER #'parse-integer))))

(-> scan-digits (state) (or null list))
(defun scan-decimal-digits (state)
  (scan-while state #'decimal-digit-p))

(defun decimal-digit-p (c)
  (and c (or (unicode-digit-p c) (eql c #\_))))

;;; ===================================================
;;; Special tokens
;;; ===================================================

(-> scan-eof (state) (or null token:token))
(defun scan-eof (state)
  (when (eofp state)
    (accept state token:@EOF)))

(-> scan-illegal (state)  token:token)
(defun scan-illegal (state)
  (advance! state)
  (accept state token:@ILLEGAL))

;;; ===================================================
;;; Utility functions
;;; ===================================================

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
  (with-slots (lexeme token-offset token-column token-line last-token) scanner
    (let* ((token-lexeme (coerce lexeme 'string))
           (value (funcall to-value token-lexeme))
           (location (make-instance 'location:source-location :line token-line :column token-column :offset token-offset)))
      (setf last-token (make-instance 'token:token :class token-class :lexeme lexeme :value value :location location))
      (prog1 last-token
        (when (token:class= last-token token:@ILLEGAL)
          (cerror "Illegal token" (make-condition 'scan-error :message "Illegal token" :location location)))))))
