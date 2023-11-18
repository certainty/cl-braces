(in-package :cl-braces.compiler.frontend.parser)

;;; This implements the parser for the highlevel language.
;;; It's a hand rolled recursive descent parser, which collects parse errors and reports them collectively at the end of the parsing process.
;;; When a parse fails, the parse will insert a sentinel node into the AST and continue parsing.
;;; The recovery is relatively simple and attempts to synchronize to the next statement boundary.

(defparameter *fail-fast* nil "If true the parser will signal a continuable parse-error condition when an error is encountered. When continued the parser will attempt to synchronize to the next statement boundary.")

(define-condition parse-errors (error)
  ((details
    :reader error-detail
    :initarg :details))
  (:documentation "The parser will collect parser errors and once the parsing process is finished it will signal this error condition containing all the details"))

(define-condition error-detail (error)
  ((location
    :reader error-location
    :initarg :location
    :type location:source-location)
   (message
    :reader error-message
    :initarg :message))
  (:report
   (lambda (condition stream)
     (let ((location (error-location condition)))
       (format stream "ParseError at Line: ~A, Column: ~A => ~A" (location:line location) (location:column location) (error-message condition)))))
  (:documentation "An instance of a parse error."))

(defclass state ()
  ((scanner
    :initarg :scanner
    :initform (error "must provide scanner")
    :type scanner:state
    :documentation "The scanner to read tokens from")
   (cur-token
    :initarg :cur-token
    :initform nil
    :type (or null token:token)
    :documentation "The most recently read token")
   (next-token
    :initarg :next-token
    :initform nil
    :type (or null token:token)
    :documentation "The next token to be read")
   (errors
    :reader parse-errors
    :initarg :errors
    :initform nil
    :type list
    :documentation "A list of parse errors that have been encountered")
   (had-errors-p
    :initarg :had-errors-p
    :initform nil
    :type boolean
    :documentation "A flag indicating if any errors have been encountered"))
  (:documentation "The state of the parser which is threaded through all parsing methods"))

(defun parse-with (input parser &rest args)
  (call-with-parse-state
   input
   (lambda (state)
     (handler-bind ((error-detail (lambda (c) (if *fail-fast* (invoke-debugger c) (invoke-restart 'continue)))))
       (advance! state)
       (apply #'funcall parser state args)))))

(-> parse (t) (values (or null ast:node) boolean state))
(defun parse (input-desginator)
  "Parses the source code denoted by `input-designator' and returns 3 values
1. the AST
2. a boolean indicating if any errors have been encountered
3. the parser state

See `scanner:source-input' for the supported input designators
Bind the dynamic variable `*fail-fast*' to true to signal a continuable parse-error condition when an error is encountered.
By default it is bound to nil, which will cause the parser to insert a sentinel node into the AST and continue parsing.
"
  (call-with-parse-state input-desginator #'%parse))


(-> %parse (state) (values (or null ast:node) boolean state))
(defun %parse (state)
  (handler-bind ((error-detail (lambda (c) (if *fail-fast* (invoke-debugger c) (invoke-restart 'synchronize)))))
    (with-slots (had-errors-p) state
      (advance! state)
      (let ((decls (loop until (eofp state) collect (parse-declaration state))))
        (consume! state token:@EOF "Expected end of file")
        (values (ast:make-program decls) had-errors-p state)))))

(defun call-with-parse-state (input-desginator fn)
  (scanner:call-with-scanner
   input-desginator
   (lambda (scanner)
     (let ((state (make-instance 'state :scanner scanner)))
       (funcall fn state)))))

;;; Every individual parse has to deal with a couple of base cases
;;; 1. The error cases
;;;     a. The current token is illegal, in which case we signal a parse-error, which will be handled at the top level declaration-parser, which attempts to synchronize to the next statement boundary.
;;;     b. The current token is EOF, in which case we signal an unexpected EOF error.
;;; 2. The parse doesn't apply
;;;    a. The parse is optional, in which case we return nil
;;;    b. The parse is mandatory, in which case we signal an error
(defmacro guard-parse (state &body body)
  `(cond
     ((illegal-token-p ,state)
      (signal-parse-error ,state "Illegal token"))
     ((eofp ,state)
      (signal-parse-error ,state "Unexpected end of file"))
     (t ,@body)))

(defun synchronize (state)

  (with-slots (cur-token) state
    (loop
      (cond
        ((eofp state)
         (return-from synchronize))
        ((token:class= cur-token token:@SEMICOLON)
         (advance! state)
         (return-from synchronize))
        ((token:class= cur-token token:@RBRACE)
         (advance! state)
         (return-from synchronize))
        (t (advance! state))))))

;; This is the main driver of the parsing process
(-> parse-declaration (state) ast:node)
(defun parse-declaration (state)
  (restart-case (%parse-declaration state)
    (synchronize ()
      :report "Record the error and attempt to resume parsing after the next statement boundary"
      (ignore-errors (synchronize state))
      (return-from parse-declaration (accept state 'ast:bad-declaration :message "Expected declaration")))))

(-> %parse-declaration (state) (or null ast:node))
(defun %parse-declaration (state)
  (guard-parse state
    (or (parse-short-variable-declaration state)
        (parse-statement state)
        (signal-parse-error state "Expected declaration"))))

(defun parse-short-variable-declaration (state)
  "Parse a short variable declaration of the form <variable> := <expression>"
  (guard-parse state
    (with-slots (cur-token next-token) state
      (when (and (token:class= cur-token token:@IDENTIFIER) (token:class= next-token token:@COLON_EQUAL))
        (let* ((ident (consume! state token:@IDENTIFIER "Expected identifier"))
               (variable (accept state 'ast:variable :identifier ident)))
          (consume! state token:@COLON_EQUAL "Expected ':='")
          (if-let ((expr (parse-expression-statement state)))
            (accept state 'ast:short-variable-declaration :variable variable :initializer expr)
            (signal-parse-error state "Expected expression")))))))

(-> parse-statement (state) (or null ast:node))
(defun parse-statement (state)
  (guard-parse state
    (or (parse-block state)
        (parse-if-statement state)
        (parse-expression-statement state))))

(defun parse-block (state)
  (guard-parse state
    (when (match-any state token:@LBRACE)
      (let ((stmts (loop until (or (eofp state) (match-any state token:@RBRACE)) collect (parse-declaration state))))
        (accept state 'ast:block :statements stmts)))))

(defun parse-if-statement (state)
  (guard-parse state
    (when-token state token:@IF
      (consume! state token:@IF "Expected 'if'")
      ;; parse conditional expression
      (let ((conditional (parse-expression state)))
        (unless conditional
          (signal-parse-error state "Expected expression in if"))
        (let ((consequence (parse-block state)))
          (unless consequence
            (signal-parse-error state "Expected block after if"))
          (unless (match-any state token:@ELSE)
            (return-from parse-if-statement (accept state 'ast:if-statement :condition conditional :consequence consequence)))
          (let ((alternative (parse-block state)))
            (unless alternative
              (signal-parse-error state "Expected block after else"))
            (accept state 'ast:if-statement :condition conditional :consequence consequence :alternative alternative)))))))

(-> parse-expression-statement (state) (or null ast:node))
(defun parse-expression-statement (state)
  (guard-parse state
    (when-let ((expr (parse-expression state)))
      (prog1 (accept state 'ast:expression-statement :expression expr)
        ;; optionally consume the semicolon
        (match-any state token:@SEMICOLON)))))

(define-enum precedence
  none
  assignment ; = or :=
  term       ; + -
  factor     ; * /
  unary      ; + - !
  primary)

(define-enum associativity
  none
  left
  right)

(define-constant +operator-rules+
    (serapeum:dict
     token:@MINUS  (cons +precedence-term+ +associativity-left+)
     token:@PLUS   (cons +precedence-term+ +associativity-left+)
     token:@SLASH  (cons +precedence-factor+ +associativity-left+)
     token:@STAR   (cons +precedence-factor+ +associativity-left+)
     token:@LT     (cons +precedence-term+ +associativity-left+)
     token:@LE     (cons +precedence-term+ +associativity-left+)
     token:@GT     (cons +precedence-term+ +associativity-left+)
     token:@GE     (cons +precedence-term+ +associativity-left+)
     token:@LPAREN (cons +precedence-none+ +associativity-none+))
  :test #'equalp)

(defun operator-rule-for (token-class)
  (gethash token-class +operator-rules+ (cons +precedence-none+ +associativity-none+)))

(defun parse-expression (state &optional (current-min-precedence +precedence-assignment+))
  "Parse the expression respecting precedence and associativity rules of the operators.
This implementation [precedence climbing algorithm](https://en.wikipedia.org/wiki/Operator-precedence_parser), to do so.
The main idea behind the algorithm is that an expression contains of groups of subexpressions that are connected by the
operator with the lowest precedence.

Example:
2 + 3 ^ 2 * 3 + 4

|---------------|   : prec 1
    |-------|       : prec 2
    |---|           : prec 3

The algorithm fits well into the overall recursive descent approach, which we take for the rest of the parser.
During recursive calls to parse subexpression we pass the current precedence level and the associativity of the operator
subexpression or to return the result to the caller.

The primary expression in this algorithm are the literals and the grouping expressions.

Example:
 ```
 (parse-expression state +precedence-assignment+)
 ```
"
  (guard-parse state
    (with-slots (cur-token) state
      (loop with left = (parse-primary-expression state)
            with right = nil
            do
               (destructuring-bind (prec . assoc) (operator-rule-for (token:class cur-token))
                 (when (< prec current-min-precedence)
                   (return left))
                 (let ((operator cur-token)
                       (next-min-precedence (if (= assoc +associativity-left+) (1+ prec) prec)))
                   (advance! state)
                   (setf right (parse-expression state next-min-precedence))
                   (setf left (accept state 'ast:binary-expression :lhs left :operator operator :rhs right))))))))

(defmacro when-token (state expected-class &body body)
  `(with-slots (cur-token) ,state
     (when (token:class= cur-token ,expected-class)
       ,@body)))

(defun parse-primary-expression (state)
  "Parse a primary expression, which are usually terminal nodes. The prefix `primary-' is often used in parsers to denot these types of productions."
  (guard-parse state
    (or
     (parse-literal state)
     (parse-unary-expression state)
     (parse-variable state)
     (parse-grouping-expression state))))

(defun parse-variable (state)
  (guard-parse state
    (when-token state token:@IDENTIFIER
      (let ((tok (consume! state token:@IDENTIFIER "Expected identifier")))
        (accept state 'ast:variable :identifier tok)))))

(defun parse-literal (state)
  "Recognizes a literal expression"
  (guard-parse state
    (or
     (parse-boolean-literal state)
     (parse-number-literal state))))

(-> parse-boolean-literal (state) (or null ast:literal))
(defun parse-boolean-literal (state)
  (guard-parse state
    (with-slots (cur-token) state
      (let ((token cur-token))
        (when (or (token:class= token token:@TRUE) (token:class= token token:@FALSE))
          (advance! state)
          (accept state 'ast:literal :token token))))))

(-> parse-number-literal (state) (or null ast:literal))
(defun parse-number-literal (state)
  (guard-parse state
    (when-token state token:@INTEGER
      (let ((tok (consume! state token:@INTEGER "Expected number literal")))
        (when tok
          (unless (token:class= tok token:@ILLEGAL)
            (accept state 'ast:literal :token tok)))))))

(defun parse-grouping-expression (state)
  (when-token state token:@LPAREN
    (let ((tok (consume! state token:@LPAREN "Expected '('")))
      (when tok
        (unless (token:class= tok token:@ILLEGAL)
          (let ((expr (parse-expression state +precedence-term+)))
            (consume! state token:@RPAREN "Expected ')' after expression")
            (accept state 'ast:grouping-expression :expression expr)))))))

(-> parse-unary-expression (state) (or null ast:expression))
(defun parse-unary-expression (state)
  "Parse a unary expression which is essentially an operator followed by a single operand, which itself could be a more complex expression"
  (guard-parse state
    (with-slots (cur-token) state
      (cond
        ((or (token:class= cur-token token:@PLUS) (token:class= cur-token token:@MINUS))
         (let ((op cur-token))
           (advance! state)
           (accept state 'ast:unary-expression :operator op :operand (parse-primary-expression state))))))))

(-> eofp (state) boolean)
(defun eofp (state)
  (with-slots (cur-token) state
    (token:class= cur-token token:@EOF)))

(defun illegal-token-p (state)
  (with-slots (cur-token) state
    (token:class= cur-token token:@ILLEGAL)))

(-> accept (state symbol &rest t) (values ast:node &optional))
(defun accept (state node-class &rest args)
  (with-slots (cur-token) state
    (if cur-token
        (apply #'make-instance node-class :location (token:location cur-token) args)
        (dev:unreachable! "No current token"))))

(-> advance! (state) (values token:token token:token))
(defun advance! (state)
  (with-slots (next-token cur-token scanner) state
    (let ((cur (scanner:next-token scanner)))
      (if (and cur-token next-token)
          (rotatef cur-token next-token cur)
          (progn
            (setf cur-token cur)
            (setf next-token (scanner:next-token scanner))))
      (when (token:class= cur-token token:@ILLEGAL)
        (signal-parse-error state "Illegal token"))
      (values cur-token next-token))))

(-> consume! (state token:token-class string &rest list) (or null token:token))
(defun consume! (state expected-token-class format-string &rest args)
  "Consumes input expecting it to be of the given token type"
  (with-slots (cur-token) state
    (assert cur-token) ; we can only get here when consume! has been called withoud advance!

    (unless (token:class= cur-token expected-token-class)
      (signal-parse-error state format-string args)
      (return-from consume! nil))
    (prog1 cur-token
      (advance! state))))

(-> match-any (state token:token-class &rest token:token-class) (or null token:token-class))
(defun match-any (state token-class &rest other-token-classes)
  "Checks if the next token matches any of the given token classes. If so it consumes the token and return true, otherwise it tries the next class."
  (with-slots (cur-token) state
    (let ((all-classes (cons token-class other-token-classes)))
      (dolist (next-class all-classes)
        (when (token:class= cur-token next-class)
          (advance! state)
          (return next-class))))))

(-> signal-parse-error (state string &rest t) null)
(defun signal-parse-error (state format-string &rest args)
  "Record an error at the current location"
  (with-slots (cur-token) state
    (apply #'signal-parse-error-at state cur-token format-string args)))

(-> signal-parse-error-at-next (state string &rest t))
(defun signal-parse-error-at-next (state format-string &rest args)
  "Record an error at the next location"
  (with-slots (next-token) state
    (signal-parse-error-at state next-token format-string args)))

(-> signal-parse-error-at (state token:token string &rest t) (or null error-detail))
(defun signal-parse-error-at (state token format-string &rest args)
  (with-slots (had-errors-p errors scanner) state
    (setf had-errors-p t)
    (let* ((loc (token:location token))
           (parse-error (make-condition 'error-detail :location loc :message (apply #'format nil format-string args))))
      (push parse-error errors)
      (error parse-error))))
