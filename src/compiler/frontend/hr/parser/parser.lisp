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
   (cur-token :initform nil :type (or null scanner:token))
   ;; one token look-ahead
   (next-token :initform nil :type (or null scanner:token))
   (errors :initform nil :reader :parse-errors)
   (had-error-p :initform nil :type boolean)
   (panic-mode-p :initform nil :type boolean)))

(defun call-with-parser (origin fn)
  (scanner:with-scanner (s origin)
    (funcall fn (make-instance 'state :scanner s))))

(-> next-node-id (state) integer)
(defun next-node-id (state)
  (with-slots (node-id) state
    (incf node-id)))

(defmacro with-parser ((parser-var origin) &body body)
  `(call-with-parser ,origin (lambda (,parser-var) ,@body)))

(-> parse (t) (values ast:source list &optional))
(defun parse (origin)
  "Parse input coming from the provided orgigin returning two values: the AST and the list of errors if there are any"
  (with-parser (p origin)
    (with-slots (errors) p
      (values (%parse p) errors))))

(-> %parse (state) ast:source)
(defun %parse (parser)
  "Parse the input and return the AST. Signals parse-errors condition if there are any errors."
  (multiple-value-bind (ast errors)
      (handler-bind ((parse-error-instance (lambda (e) (if *fail-fast* (invoke-debugger e) (invoke-restart 'continue)))))
        (advance! parser)
        (parse-declaration parser))
    (when (consp errors)
      (error 'parse-errors :errors errors))
    ast))

(-> eof-p (state) boolean)
(defun eof-p (parser)
  (with-slots (cur-token) parser
    (scanner:token-eof-p cur-token)))

(-> parse-declaration (state) ast:declaration)
(defun parse-declaration (parser)
  (with-slots (had-error-p) parser
    (let ((statement (parse-statement parser)))
      (when had-error-p
        (synchronize! parser)
        (return-from parse-declaration (accept parser 'ast:bad-declaration)))
      statement)))

(-> parse-statement (state) ast:statement)
(defun parse-statement (parser)
  (parse-expression-statement parser))

(-> parse-expression-statement (state) ast:statement)
(defun parse-expression-statement (parser)
  (let ((expr (parse-expression parser)))
    (accept parser 'ast:expression-statement :expression expr)))

(-> parse-expression (state) ast:expression)
(defun parse-expression (parser)
  (or (parse-number-literal parser)
      (accept parser 'ast:bad-expression)))

(-> parse-number-literal (state) (or null ast:expression))
(defun parse-number-literal (parser)
  (let ((tok (consume! parser :tok-integer "Expected number literal")))
    (unless (scanner:token-illegal-p tok)
      (accept parser 'ast:literal-expression :token tok))))

(-> parse-identifier (state) (or null ast:identifier))
(defun parse-identifier (parser)
  (multiple-value-bind (tok had-error-p) (consume! parser :tok-identifier "Expected identifier")
    (unless had-error-p
      (accept parser 'ast:identifier :name (scanner:token-value tok)))))

(-> accept (state symbol &rest t) (values ast:node &optional))
(defun accept (parser node-class &rest args)
  (with-slots (cur-token) parser
    (if cur-token
        (apply #'make-instance node-class :id (next-node-id parser) :location (scanner:token-location cur-token) args)
        (unreachable! "No current token"))))

(-> consume! (state scanner:tpe-token string &rest t) scanner:token)
(defun consume! (parser expected-token-type format-string &rest args)
  "Consumes input expecting it to be of the given token type"
  (with-slots (cur-token) parser
    (assert cur-token)
    (unless (eql (scanner:token-type cur-token) expected-token-type)
      (error-at-current parser format-string args))
    (prog1 cur-token
      (advance! parser))))

(-> advance! (state) (values scanner:token scanner:token))
(defun advance! (parser)
  "Advance in the token screen setting the internal state of the parser accordingly."
  (with-slots (next-token cur-token scanner) parser
    (let ((cur (scanner:next-token scanner)))
      (if (and cur-token next-token)
          (rotatef cur-token next-token cur)
          (progn
            (setf cur-token cur)
            (setf next-token (scanner:next-token scanner))))
      (when (scanner:token-illegal-p cur-token)
        (error-at-current parser "Illegal token"))
      (values cur-token next-token))))

(-> skip-illegal! (state) (values scanner:token boolean))
(defun skip-illegal! (parser)
  "Reads the next available legal token, skipping illegal ones as we go. Each illegal token will be recorded as an error "
  (with-slots (cur-token had-error-p) parser
    (let ((had-illegal nil))
      (loop
        (advance! parser)
        (cond
          ((eof-p parser) (return))
          ((scanner:token-illegal-p cur-token)
           (setf had-error-p t))
          (t (return))))
      (values cur-token had-illegal))))

(-> synchronize! (state) scanner:token)
(defun synchronize! (parser)
  "Attempt to find a synchronization point in the input stream, at which we can attempt to continue parsing.
The parse will likely generate a couple of invalid nodes."
  ;; find next legal token
  (with-slots (panic-mode-p next-token cur-token) parser
    (setf panic-mode-p nil)
    (loop
      (let ((cur-token-type (and cur-token (scanner:token-type cur-token)))
            (next-token-type (and next-token parser (scanner:token-type next-token))))
        (cond
          ((eof-p parser) (return))
          ((member cur-token-type (list :tok-semicolon :tok-rbrace))
           (advance! parser)
           (return))
          ((member next-token-type (list :tok-kw-func :tok-kw-if :tok-kw-for)) (return))
          (t (skip-illegal! parser)))))
    cur-token))

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
