(in-package :cl-braces.compiler.frontend.parser)

;;; This implements the parser for the highlevel language.
;;; It's a hand rolled recursive descent parser, which collects parse errors and reports them collectively at the end of the parsing process.
;;; When a parse fails, the parse will insert a sentinel node into the AST and continue parsing.
;;; The recovery is relatively simple and attempts to synchronize to the next statement boundary.

(defmacro define-enum (name &rest variants)
  (let ((iota 0))
    `(progn
       ,@(mapcar (lambda (variant)
                   (prog1 `(defconstant ,(intern (format nil "+~A-~A+" name variant)) ,iota)
                     (incf iota)))
                 variants)
       (deftype ,(intern (format nil "~A" name)) () '(integer 0 ,iota)))))

(defparameter *fail-fast* nil "If true the parser will signal a continuable parse-error condition when an error is encountered. When continued the parser will attempt to synchronize to the next statement boundary.")

(define-condition parse-errors (error)
  ((details :reader error-detail
            :initarg :details))
  (:documentation "The parser will collect parser errors and once the parsing process is finished it will signal this error condition containing all the details"))

(define-condition error-detail (error)
  ((location :reader error-location
             :initarg :location
             :type token:location)
   (message :reader error-message
            :initarg :message))
  (:report
   (lambda (condition stream)
     (let ((location (error-location condition)))
       (format stream "ParseError at Line: ~A, Column: ~A => ~A" (token:location-line location) (token:location-column location) (error-message condition)))))
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
    :initarg :errors
    :initform nil
    :type list
    :documentation "A list of parse errors that have been encountered")
   (had-errors-p
    :initarg :had-errors-p
    :initform nil
    :type boolean
    :documentation "A flag indicating if any errors have been encountered")
   (panic-mode-p
    :initarg :panic-mode-p
    :initform nil
    :type boolean
    :documentation "A flag indicating if the parser is in panic mode"))
  (:documentation "The state of the parser which is threaded through all parsing methods"))

(defun parse-with (input parser &rest args)
  (call-with-parse-state
   input
   (lambda (state)
     (handler-bind ((error-detail (lambda (c) (if *fail-fast* (invoke-debugger c) (invoke-restart 'continue)))))
       (advance! state)
       (apply #'funcall parser state args)))))

(defun parse (input-desginator)
  "Parses the source code denoted by `input-designatore' and returns 3 values
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
  (handler-bind ((error-detail (lambda (c) (if *fail-fast* (invoke-debugger c) (invoke-restart 'continue)))))
    (with-slots (had-errors-p) state
      (advance! state) ; prime the state
      (let ((ast (parse-expression state)))
        (consume! state token:@EOF "Expected end of file")
        (values ast had-errors-p state)))))

(defun call-with-parse-state (input fn)
  (scanner:call-with-scanner
   input
   (lambda (scanner)
     (let ((state (make-instance 'state :scanner scanner)))
       (funcall fn state)))))

(define-enum precedence
  none
  assignment ; =
  term       ; + -
  factor     ; * /
  unary      ; + - !
  primary)

(define-enum associativity
  none
  left
  right)

(serapeum:defconst +operator-rules+
  (serapeum:dict
   token:@MINUS  (cons +precedence-term+ +associativity-left+)
   token:@PLUS   (cons +precedence-term+ +associativity-left+)
   token:@SLASH  (cons +precedence-factor+ +associativity-left+)
   token:@STAR   (cons +precedence-factor+ +associativity-left+)
   token:@LPAREN (cons +precedence-none+ +associativity-none+)))

(defun operator-rule-for (token-class)
  (gethash token-class +operator-rules+ (cons +precedence-none+ +associativity-none+)))

(defun parse-expression (state &optional (current-min-precedence +precedence-assignment+))
  "Parse and expression respecting precedence and associativity rules of the operators.
It is implemented using the [precedence climbing algorithm](https://en.wikipedia.org/wiki/Operator-precedence_parser).
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

The primary expressio in this algorithm are the literals and the grouping expressions.
"
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
                 (setf left (accept state 'ast:binary-expression :lhs left :operator operator :rhs right)))))))

(defun parse-primary-expression (state)
  (or
   (parse-literal state)
   (parse-unary-expression state)
   (parse-grouping-expression state)))

(defun parse-literal (state)
  (or (parse-number-literal state)))

(-> parse-number-literal (state) (or null ast:literal))
(defun parse-number-literal (state)
  (let ((tok (consume! state token:@INTEGER "Expected number literal")))
    (when tok
      (unless (token:class= tok token:@ILLEGAL)
        (accept state 'ast:literal :token tok)))))

(defun parse-grouping-expression (state)
  (let ((tok (consume! state token:@LPAREN "Expected '('")))
    (when tok
      (unless (token:class= tok token:@ILLEGAL)
        (let ((expr (parse-expression state +precedence-term+)))
          (consume! state token:@RPAREN "Expected ')' after expression")
          (accept state 'ast:grouping-expression :expression expr))))))

(-> parse-unary-expression (state) (or null ast:expression))
(defun parse-unary-expression (state)
  "Parse a unary expression which is essentially an operator followed by a single operand, which itself could be a more complex expression"
  (with-slots (cur-token) state
    (cond
      ((or (token:class= cur-token token:@PLUS) (token:class= cur-token token:@MINUS))
       (let ((op cur-token))
         (advance! state)
         (accept state 'ast:unary-expression :operator op :operand (parse-primary-expression state)))))))

(-> eofp (state) boolean)
(defun eofp (state)
  (with-slots (cur-token) state
    (token:class= cur-token token:@EOF)))

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
  (with-slots (panic-mode-p had-errors-p errors scanner) state
    (unless panic-mode-p
      (setf panic-mode-p t)
      (setf had-errors-p t)
      (let* ((loc (token:location token))
             (parse-error (make-condition 'error-detail :location loc :message (apply #'format nil format-string args))))
        (cerror "Continue parsing collecting this error" parse-error)
        (prog1 parse-error
          (push parse-error errors))))))
