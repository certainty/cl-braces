(in-package :cl-braces.compiler.frontend.parser)

;;; This implements the parser for the highlevel language.
;;; It's a hand rolled recursive descent parser, which collects parse errors and reports them collectively at the end of the parsing process.
;;; When a parse fails, the parse will insert a sentinel node into the AST and continue parsing.
;;; The recovery is relatively simple and attempts to synchronize to the next statement boundary.

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

(defun parse (input-desginator)
  (scanner:call-with-scanner
   input-desginator
   (lambda (scanner)
     (let ((state (make-instance 'state :scanner scanner)))
       (%parse state)))))

(-> %parse (state) (or null ast:node))
(defun %parse (state)
  (handler-bind ((error-detail (lambda (c) (if *fail-fast* (invoke-debugger c) (invoke-restart 'continue)))))
    (advance! state)
    (prog1 (parse-expression state)
      (consume! state token:@EOF "Expected end of file"))))

(defun parse-expression (state)
  (or
   (parse-unary-expression state)
   (parse-literal state)))

(defun parse-literal (state)
  (or (parse-number-literal state)))

(-> parse-number-literal (state) (or null ast:literal))
(defun parse-number-literal (state)
  (let ((tok (consume! state token:@INTEGER "Expected number literal")))
    (unless (token:class= tok token:@ILLEGAL)
      (accept state 'ast:literal :token tok))))

(-> parse-unary-expression (state) (or null ast:expression))
(defun parse-unary-expression (state)
  "Parse a unary expression which is essentially an operator followed by a single operand, which itself could be a more complex expression"
  (with-slots (cur-token) state
    (cond
      ((or (token:class= cur-token token:@PLUS) (token:class= cur-token token:@MINUS))
       (let ((op cur-token))
         (advance! state)
         (accept state 'ast:unary-expression :operator op :operand (parse-expression state)))))))

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

(-> consume! (state token:token-class string &rest list) token:token)
(defun consume! (state expected-token-class format-string &rest args)
  "Consumes input expecting it to be of the given token type"
  (with-slots (cur-token) state
    (assert cur-token) ; we can only get here when consume! has been called withoud advance!

    (unless (token:class= cur-token expected-token-class)
      (signal-parse-error state format-string args))
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

(-> signal-parse-error-at (state token:token string &rest t) null)
(defun signal-parse-error-at (state token format-string &rest args)
  (with-slots (panic-mode-p had-errors-p errors scanner) state

    (when panic-mode-p (return-from signal-parse-error-at))

    (setf panic-mode-p t)
    (setf had-error-p t)

    (let* ((loc (token:location token))
           (parse-error (make-condition 'error-detail :location loc :message (apply #'format nil format-string args))))
      (cerror "Continue parsing collecting this error" parse-error)
      (push parse-error errors))))
