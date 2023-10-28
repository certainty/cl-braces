(in-package :cl-braces.compiler.frontend.parser)

(defparameter *fail-fast* nil "If true the parser will signal a continuable parse-error condition when an error is encountered. If false the parser will attempt to synchronize and continue parsing automatically.")

(define-condition parse-errors (error)
  ((errors :initarg :errors :reader parse-error-instances))
  (:documentation "When parsing fails this error is raised. It contains all the parse errors that occured during parsing"))

(define-condition parse-error-instance (error)
  ((origin :initarg :origin :reader parse-error-origin)
   (location :initarg :location :reader parse-error-location)
   (message :initarg :message :reader parse-error-message))
  (:report
   (lambda (condition stream)
     (let ((location (parse-error-location condition)))
       (format stream "ParseError in ~A at Line: ~A, Column: ~A => ~A" (scanner:source-uri (parse-error-origin condition)) (scanner:location-line location) (scanner:location-column location) (parse-error-message condition)))))
  (:documentation "A parse error."))

(defclass state ()
  ((scanner :initform (error "No scanner provided") :initarg :scanner :type scanner:state)
   (node-id :initform 0)
   (prev-token :initform nil :type (or null scanner:token))
   (cur-token :initform nil :type (or null scanner:token))
   (errors :initform nil :reader :parse-errors)
   (had-error-p :initform nil :type boolean)
   (panic-mode-p :initform nil :type boolean)))

(defun call-with-parser (origin fn)
  (scanner:with-scanner (s origin)
    (funcall fn (make-instance 'state :scanner s))))

(defun next-node-id (state)
  (with-slots (node-id) state
    (incf node-id)))

(defmacro with-parser ((parser-var origin) &body body)
  `(call-with-parser ,origin (lambda (,parser-var) ,@body)))

(defun parse (origin)
  "Parse input coming from the provided orgigin returning two values: the AST and the list of errors if there are any"
  (with-parser (p origin)
    (with-slots (errors) p
      (values (%parse p) errors))))

(-> do-parse (parse-state) ast:source)
(defun %parse (parser)
  "Parse the input and return the AST. Signals parse-errors condition if there are any errors."
  (multiple-value-bind (ast errors)
      (handler-bind ((parse-error-instance (lambda (e) (if *fail-fast* (invoke-debugger e) (invoke-restart 'continue)))))
        (advance! parser)
        (parse-declaration parser))
    (when (consp errors)
      (error 'parse-errors :errors errors))
    ast))

(defun eof-p (parser)
  (with-slots (cur-token) parser
    (scanner:token-eof-p cur-token)))

(defun parse-declaration (parser)
  (with-slots (had-error-p) parser
    (let ((statement (parse-statement parser)))
      (when had-error-p
        (synchronize! parser)
        (return-from parse-declaration (accept parser 'ast:bad-declaration)))
      statement)))

(defun parse-statement (parser)
  (parse-expression-statement parser))

(defun parse-expression-statement (parser)
  (with-slots (had-error-p) parser
    (let ((expr (parse-expression parser)))
      (if had-error-p
          (accept parser 'ast:bad-statement)
          (accept parser 'ast:expression-statement :expression expr)))))

(defun parse-expression (parser)
  (parse-number-literal parser))

(defun parse-number-literal (parser)
  (with-slots (had-error-p) parser
    (let ((tok (consume! parser :tok-number "Expected number literal")))
      (if had-error-p
          (accept parser 'ast:bad-expression)
          (accept parser 'ast:literal-expression :token tok)))))

(defun accept (parser node-class &rest args)
  (with-slots (cur-token) parser
    (if cur-token
        (apply #'make-instance node-class :id (next-node-id parser) :location (scanner:token-location cur-token) args)
        (unreachable! "No current token"))))

(defun parse-identifier (parser)
  (multiple-value-bind (tok had-error-p) (consume! parser :tok-identifier "Expected identifier")
    (if had-error-p
        (accept parser 'ast:bad-expression)
        (accept parser 'ast:identifier :name (scanner:token-value tok)))))

;; TODO: think about the invariants I want to have from consume and the internal error state
(defun consume! (parser expected-token-type format-string &rest args)
  "Consumes input expecting it to be of the given token type"
  (with-slots (cur-token had-error-p) parser
    (unless (eql (scanner:token-type cur-token) expected-token-type)
      (error-at-current parser format-string args))
    (prog1 (scanner:token-illegal-p cur-token)
      (advance! parser))))

(defun advance! (parser)
  "Reads the next legal token. If an illegal token has been encountered it is recorded as an error.
Returns two values:
1. the token
2. had-error-p indicating an error
"
  (with-slots (had-error-p prev-token cur-token scanner) parser
    (setf prev-token cur-token)
    (setf cur-token (scanner:next-token scanner))
    (when (scanner:token-illegal-p cur-token)
      (error-at-current "Illegal token ~A" cur-token))
    (values cur-token had-error-p)))

(defun skip-illegal! (parser)
  "Reads the next available legal token, skipping illegal ones as we go. Each illegal token will be recorded as an error.
Returns two values:
1. the next legal token (which might be the eof-token)
2. had-error-p indicating an error
"
  (with-slots (cur-token had-error-p) parser
    (loop (advance! parser) while (scanner:token-illegal-p cur-token))
    (values cur-token had-error-p)))

(defun synchronize! (parser)
  "Attempt to find a synchronization point in the input stream, at which we can attempt to continue parsing.
The parse will likely generate a couple of invalid nodes."
  ;; find next legal token
  (with-slots (panic-mode-p prev-token cur-token) parser
    (setf panic-mode-p nil)
    (loop
      (let ((prev-token-type (and prev-token (scanner:token-type prev-token)))
            (cur-token-type (and cur-token parser (scanner:token-type cur-token))))
        (cond
          ((eof-p parser) (return))
          ((eql prev-token-type :tok-semicolon) (return))
          ((member cur-token-type (list :tok-rbrace :tok-kw-func :tok-kw-if :tok-kw-for)) (return))
          (t (skip-illegal! parser)))))))

(defun error-at-current (parser format-string &rest args)
  "Record an error at the current location"
  (with-slots (cur-token) parser
    (apply #'error-at parser cur-token format-string args)))

(defun error-at (parser token format-string &rest args)
  "Record an error at the given location. This function signals a continuable parse-error condition."
  (with-slots (panic-mode-p had-error-p errors scanner) parser
    (when panic-mode-p (return-from error-at))

    (setf panic-mode-p t)
    (setf had-error-p t)

    (let* ((loc (scanner:token-location token))
           (origin (scanner:scan-origin scanner))
           (parse-error (make-condition 'parse-error-instance :origin origin :location loc :message (apply #'format nil format-string args))))
      (cerror "Continue parsing collecting this error" parse-error)
      (push parse-error errors))))
