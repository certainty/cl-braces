(in-package :cl-braces.compiler.frontend.scanner)
;;;;
;;;; The scanner is the first stage of the compiler. It takes a stream of characters and produces a stream of tokens.
;;;; A token is a representation of a lexeme in the input stream bundled with some metadata about the location and the class of the lexeme.
;;;; The token's class is used by the parser to determine how to interpret the token.
;;;;
;;;; In many sources you will see that lexers use regular expression, even maybe using either builtin or external regexp engines to do that.
;;;; However this has tow main downsides:
;;;; 1. This usually doesn't support location tracking
;;;; 2. This will get very tricky when you want to support constructs that require counting, like strings with escaped characters and more complicated types of character literals.
;;;;
;;;; It's very possible to do these things with regular expressions but then the simplicity of the approach begins to break down.
;;;; This scanner implements only the subset of regular expressions that are required to scan the golang language.
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
  ((source-code
    :initarg :source-code
    :initform (error "no source code given")
    :type string
    :documentation "The source code that's being scanned")

   (cursor-base
    :initform 0
    :type a:array-index
    :documentation "The offset in the input stream")

   (cursor-advanced
    :initform 0
    :type a:array-index
    :documentation "The offset in the input stream")

   (location-base
    :initform (make-instance 'location:source-location :line 1 :column 1 :offset 0)
    :type location:source-location
    :documentation "The location in the input stream")

   (last-token
    :initform nil
    :type (or null token:token)
    :documentation "The preview token that was scanned. This is required to correctly inject semicolons when necessary.")

   (had-errors
    :initform nil
    :type boolean
    :documentation "If true then the scanner has encountered an error and will not continue to scan.")

   (fail-fast
    :initarg :fail-fast
    :initform nil
    :type boolean
    :documentation "If true then the scanner will signal a `scan-error' condition when it encounters an illegal token.
                     If false then the scanner will try to recover from an illegal token by skipping over it and continuing to scan.")))

;;; ================================
;;; Main API
;;; ================================


(-> make-scanner (string &key (:fail-fast boolean)) state)
(defun make-scanner (source-code &key (fail-fast nil))
  "Creates a new scanner that is initialized with the given `source-code'.
   If `fail-fast' is true then the scanner will signal a `scan-error' condition when it encounters an illegal token.
   Returns a fresh instance of `scanner'."
  (make-instance 'state :source-code source-code :fail-fast fail-fast))

(-> scan-all (string &key (:fail-fast boolean)) (values (vector token:token *) boolean))
(defun scan-all (source-code &key (fail-fast nil))
  "Scans the given `source-code' and returns a list of all the tokens that were scanned.
   If `fail-fast' is true then the scanner will signal a `scan-error' condition when it encounters an illegal token.
   If `fail-fast' is false then the scanner will try to recover from an illegal token by skipping over it and continuing to scan."

  (let ((scanner (make-scanner source-code :fail-fast fail-fast))
        (result  (make-array 0 :adjustable t :fill-pointer 0 :element-type 'token:token)))

    (loop for token = (next-token scanner)
          do (vector-push-extend token result)
          when (token:class= (slot-value scanner 'last-token) token:@EOF)
            return (values result (slot-value scanner 'had-errors)))))

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
   ```"
  (with-slots (fail-fast) state
    (handler-bind ((scan-error (lambda (e)
                                 (if fail-fast
                                     (invoke-debugger e)
                                     (when (find-restart 'continue)
                                       (invoke-restart 'continue))))))
      (scan-token state))))

(-> scan-token (state) token:token)
(defun scan-token (state)
  (or (scan-inter-token-space state)
      (scan-eof state)
      (scan-literal state)
      (scan-operator/punctuation state)
      (scan-identifier state)
      (scan-illegal state)))

;;; ============================================================================
;;; Whitespace, comments and semicolons
;;;
;;; https://golang.org/ref/spec#Comments
;;; https://golang.org/ref/spec#Semicolons
;;; ============================================================================

(a:define-constant +whitespace+ (list #\Space #\Tab #\Return #\Newline) :test #'equalp)

(-> inter-token-space-p (character) boolean)
(defun inter-token-space-p (c)
  "Returns `t' if `c' is considered inter-token whitespace.
   - space (U+0020)
   - horizontal tab (U+0009)
   - carriage return (U+000D)
   - newlines (U+000A)"
  (s:true (member c +whitespace+)))

(-> scan-inter-token-space (state) (or null token:token))
(defun scan-inter-token-space (state)
  (with-slots (cursor-base cursor-advanced location-base) state
    (when (inject-semicolon-p state)
      (return-from scan-inter-token-space (accept state token:@SEMICOLON :synthetic t)))

    ;; first skip over whitespace
    (scan-while state #'inter-token-space-p)
    (skip state)

    ;; then skip over comments
    (when (peek= state "//")
      (advance state)
      (advance state)
      (scan-until state #'(lambda (c) (eql c #\Newline)))
      (skip state))))

;;; ===================================================
;;; Literals
;;;
;;; https://golang.org/ref/spec#Literal
;;; ===================================================

(-> scan-literal (state) (or null token:token))
(defun scan-literal (state)
  (scan-integer state))

(-> scan-integer (state) (or null token:token))
(defun scan-integer (state)
  "Scans and integer literal."
  (scan-decimal-literal state))

(-> scan-decimal-literal (state) (or null token:token))
(defun scan-decimal-literal (state)
  (let ((first-digit (current-char state)))
    (when (eql first-digit #\0)
      (advance state)
      (return-from scan-decimal-literal (accept state token:@INTEGER :coerce-with (constantly 0))))
    (when (decimal-digit-p first-digit)
      (advance state)
      (scan-while state #'decimal-digit-p)
      (accept state token:@INTEGER :coerce-with #'parse-integer))))

(-> decimal-digit-p (character) (or null boolean))
(defun decimal-digit-p (c)
  (and c (s:true (unicode-digit-p c))))

;;; ===================================================
;;; Identifier
;;;
;;; https://golang.org/ref/spec#Identifiers
;;;
;;; A keyword is an identifier with one of the following lexemes:
;;; break        default      func         interface    select
;;; case         defer        go           map          struct
;;; chan         else         goto         package      switch
;;; const        fallthrough  if           range        type
;;; continue     for          import       return       var
;;; ==================================================
;;;
;;; This function further classifies the identifier into one of the following:
;;; - nil
;;; - true
;;; - false
;;; - keyword
;;; - identifier
;;;
;;; Deviation from the golang specification in that it recognizes nil, true and false as literals.
;;; Golang uses predeclared constants for those.
;;; ===================================================

(a:define-constant +keywords+
    ;; dictionary mapping keyword lexemes to their class
    (s:dict
     "func" token:@FUNC
     "if" token:@IF
     "else" token:@ELSE
     "break" token:@BREAK
     "continue" token:@CONTINUE
     "fallthrough" token:@FALLTHROUGH
     "return" token:@RETURN
     "var" token:@VAR)
  :test #'equalp)

(-> scan-identifier (state) (or null token:token))
(defun scan-identifier (state)
  "Scan an identifier which is either a user-defined identifier, a literal or a keyword."
  (a:when-let ((first-char (current-char state)))
    (when (letter-p first-char)
      (advance state)
      (scan-while state #'identifier-char-p)
      (let ((identifier (current-lexeme state)))
        (cond
          ((string= identifier "nil") (accept state token:@NIL :coerce-with (constantly 'none)))
          ((string= identifier "true") (accept state token:@TRUE :coerce-with (constantly t)))
          ((string= identifier "false") (accept state token:@FALSE :coerce-with (constantly nil)))
          (t (a:if-let ((keyword (gethash identifier +keywords+)))
               (accept state keyword)
               (accept state token:@IDENTIFIER))))))))

(-> identifier-char-p (character) boolean)
(defun identifier-char-p (c)
  (s:true
   (or (letter-p c)
       (unicode-digit-p c))))

(-> letter-p (character) boolean)
(defun letter-p (c)
  "Returns true if `c' is a letter according to the golang language specification"
  (s:true (and c (or (unicode-letter-p c) (eql c #\_)))))

(-> unicode-letter-p (character) boolean)
(defun unicode-letter-p (c)
  "Returns true if `c' is a unicode letter according to the golang language specification"
  (s:true (and c (sb-unicode:alphabetic-p c))))

(-> unicode-digit-p (character) boolean)
(defun unicode-digit-p (c)
  "Returns true if `c' is a unicode digit according to the golang language specification"
  (s:true (and c (digit-char-p c))))

;;; ===================================================
;;; Operators and punctuation
;;;
;;; https://golang.org/ref/spec#Operators_and_punctuation
;;; ===================================================

(-> scan-operator/punctuation (state) (or null token:token))
(defun scan-operator/punctuation (state)
  (cond
    ((match= state "...") (accept state token:@ELLIPSIS))
    ((match= state ":=") (accept state token:@COLON_EQUAL))
    ((match= state "==") (accept state token:@EQUAL_EQUAL))
    ((match= state "<=") (accept state token:@LE))
    ((match= state ">=") (accept state token:@GE))
    ((match= state "--") (accept state token:@MINUS_MINUS))
    ((match= state "++") (accept state token:@PLUS_PLUS))
    ((match= state "+=") (accept state token:@PLUS_EQUAL))
    ((match= state "*=") (accept state token:@MUL_EQUAL))
    ((match= state "(") (accept state token:@LPAREN))
    ((match= state ")") (accept state token:@RPAREN))
    ((match= state "{") (accept state token:@LBRACE))
    ((match= state "}") (accept state token:@RBRACE))
    ((match= state "[") (accept state token:@LBRACKET))
    ((match= state "]") (accept state token:@RBRACKET))
    ((match= state "+") (accept state token:@PLUS))
    ((match= state "-") (accept state token:@MINUS))
    ((match= state "/") (accept state token:@SLASH))
    ((match= state "*") (accept state token:@STAR))
    ((match= state ";") (accept state token:@SEMICOLON))
    ((match= state ",") (accept state token:@COMMA))
    ((match= state ".") (accept state token:@DOT))
    ((match= state "<") (accept state token:@LT))
    ((match= state ">") (accept state token:@GT))
    ((match= state "=") (accept state token:@EQUAL))))

(-> scan-eof (state) (or null token:token))
(defun scan-eof (state)
  "Returns the `token:@EOF' token if the end of the input stream has been reached."
  (with-slots (cursor-advanced source-code) state
    (when (eofp state)
      (setf cursor-advanced (length source-code))
      (return-from scan-eof (accept state token:@EOF)))))

(-> scan-illegal (state) (or null token:token))
(defun scan-illegal (state)
  (advance state)
  (accept state token:@ILLEGAL))

;;; Inject semicolons
;;; https://golang.org/ref/spec#Semicolons

(-> inject-semicolon-p (state) boolean)
(defun inject-semicolon-p (state)
  "Returns `t' if the scanner should inject a semicolon at the current position in the input stream.
   This is decided according to the [golang language specification](https://golang.org/ref/spec#Semicolons)."
  (with-slots (last-token) state
    (when (and (peek= state #\Newline) last-token)
      (or
       (token:literal-p last-token)
       (token:identifier-p last-token)
       (token:class-any-p last-token token:@BREAK token:@CONTINUE token:@FALLTHROUGH token:@RETURN)
       (token:class-any-p last-token token:@PLUS_PLUS token:@MINUS_MINUS token:@RPAREN token:@RBRACKET token:@RBRACE)))))

(-> token:literal-p (token) boolean)
(defun eofp (state)
  "Returns `t' if the scanner has reached the end of the input stream."
  (with-slots (cursor-advanced source-code) state
    (>= cursor-advanced (length source-code))))

(-> match= (state (or string character)) boolean)
(defun match= (state p)
  "Returns `t' if the next characters in the input stream matches `p' and consumes them"
  (when (peek= state p)
    (prog1 t
      (etypecase p
        (string
         (dotimes (i (length p))
           (declare (ignore i))
           (advance state)))
        (character (advance state))))))

(-> peek= (state (or string character)) boolean)
(defun peek= (state p)
  "Returns `t' if the next characters in the input stream matches  `p' without advancing the scanner.
   `p' can be either a string or a character.

   Examples:

   ```common-lisp
    (peek= state #\Newline)
    (peek= state \"//\")
   ```
  "
  (with-slots (cursor-advanced source-code) state
    (etypecase p
      (string (let ((end (+ cursor-advanced (length p))))
                (when (<= end (length source-code))
                  (string= p (subseq source-code cursor-advanced end)))))
      (character (eql (current-char state) p)))))

(-> accept (state token:token-class &key (:coerce-with (function (string) *)) (:synthetic boolean)) token:token)
(defun accept (state token-class &key (coerce-with #'identity) (synthetic nil))
  "Accept the scanned lexeme and constructs a `token:token' from it using the provided `token-class' "
  (with-slots (cursor-base cursor-advanced location-base source-code last-token had-errors) state
    (let* ((lexeme (subseq source-code cursor-base cursor-advanced))
           (span (update-locations state)))

      (setf cursor-base cursor-advanced)
      (setf last-token (make-instance 'token:token
                                      :class token-class
                                      :lexeme (if synthetic "synth" lexeme)
                                      :span span
                                      :value (funcall coerce-with lexeme)))
      (prog1 last-token
        (when (token:class= last-token token:@ILLEGAL)
          (setf had-errors t)
          (cerror "Illegal token" (make-condition 'scan-error :message "Illegal token" :location (span:from span))))))))

(-> current-char (state) (or null character))
(defun current-char (state)
  (with-slots (cursor-advanced source-code) state
    (unless (eofp state)
      (aref source-code cursor-advanced))))

(-> current-lexeme (state) (or null string))
(defun current-lexeme (state)
  (with-slots (cursor-base cursor-advanced source-code) state
    (subseq source-code cursor-base cursor-advanced)))

;;; TODO: improve the code for the location tracking. This is really quite difficult to get right.
(-> update-locations (state) span:source-span)
(defun update-locations (state)
  "Updates the internal location state and returns the span for the current lexeme."
  (with-slots (cursor-base cursor-advanced location-base source-code) state
    (let* ((lexeme (subseq source-code cursor-base cursor-advanced))
           (len (length lexeme))
           (end-line (location:line location-base))
           (end-column (+ (1- len) (location:column location-base)))
           (end-offset (+ (1- len) (location:offset location-base)))
           (end-location (location:make-source-location end-line end-column end-offset))
           (span (span:make-span location-base end-location))
           (newlines (newline-positions lexeme)))
      (prog1 span
        (if (null newlines)
            (setf location-base (location:make-source-location
                                 end-line
                                 (incf end-column)
                                 (incf end-offset)))
            (setf location-base (location:make-source-location
                                 (+ end-line (length newlines))
                                 (- len (car newlines))
                                 (incf end-offset))))))))

(-> newline-positions (string) list)
(defun newline-positions (lexeme)
  "Returns a list of positions in `lexeme' where a newline character is found."
  (loop for i from 0 below (length lexeme)
        when (eql (aref lexeme i) #\Newline)
          collect i))

(-> scan-while (state (function (character) boolean)) (or null character))
(defun scan-while (state predicate)
  "Consume the input, returning the list of consumed characters, while `predicate' returns `t'"
  (loop for c = (current-char state)
        while (and c (funcall predicate c))
        do (advance state)))

(-> scan-until (state (function (character) boolean)) (or null character))
(defun scan-until (state predicate)
  "Consume the input, returning the list of consumed characters, until `predicate' returns `t'"
  (loop for c = (current-char state)
        while (and c (not (funcall predicate c)))
        do (advance state)))

(-> skip (state) null)
(defun skip (state)
  "Skip the scanned lexeme without constructing a token from it."
  (with-slots (cursor-base cursor-advanced location-base) state
    (prog1 nil
      (update-locations state)
      (setf cursor-base cursor-advanced))))

(-> advance (state) (or null character))
(defun advance (state)
  "Advance the scanner to the next character, adjusting internal state to keep track of location in the input stream.
   Returns the character that was advanced to or nil if the end of the input has been reached."
  (with-slots (cursor-advanced source-code) state
    (when (>= cursor-advanced (length source-code))
      (return-from advance nil))

    (s:lret ((current-char (aref source-code cursor-advanced)))
      (incf cursor-advanced))))
