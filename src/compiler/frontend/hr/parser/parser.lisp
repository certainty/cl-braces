(in-package :cl-braces.compiler.frontend.parser)

(defparameter *parser-fail-fast* nil "If true the parser will signal a continuable parse-error condition when an error is encountered. If false the parser will attempt to synchronize and continue parsing automatically.")

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

(defstruct (parse-state (:conc-name parser-) (:constructor make-parser (provided-scanner)))
  (scanner provided-scanner :type scanner:scan-state)
  (prev-token nil :type (or null scanner:token))
  (cur-token nil :type (or null scanner:token))
  (errors nil)
  (had-error-p nil :type boolean)
  (panic-mode-p nil :type boolean))

(defun call-with-parser (origin fn)
  (scanner:with-scanner (s origin)
    (funcall fn (make-parser s))))

(defmacro with-parser ((parser-var origin) &body body)
  `(call-with-parser ,origin (lambda (,parser-var) ,@body)))

(defun parse (origin)
  "Parse input coming from the provided orgigin returning two values: the AST and the list of errors if there are any"
  (with-parser (p origin)
    (do-parse p)))

(-> do-parse (parse-state) ast:source)
(defun do-parse (parser)
  "Parse the input and return the AST. Signals parse-errors condition if there are any errors."
  (setf ast:*node-id-counter* 1)
  (multiple-value-bind (ast errors)
      (handler-bind ((parse-error-instance (lambda (e) (if *parser-fail-fast* (invoke-debugger e) (invoke-restart 'continue))))
                     (scanner:scan-error (lambda (e) (if *parser-fail-fast* (invoke-debugger e) (invoke-restart 'continue)))))
        (parse-source parser))
    (when (consp errors)
      (error 'parse-errors :errors errors))
    ast))

(defun parser-eof-p (parser)
  (scanner:token-eof-p (parser-cur-token parser)))

(-> parse-source (parse-state) (values (or null ast:source) list))
(defun parse-source (parser)
  "Parse a source file and return two values: the AST and a list of errors."
  (advance! parser)
  (let ((decls (loop for decl = (parse-declaration parser)
                     collect decl
                     until (parser-eof-p parser))))
    (if (parser-had-error-p parser)
        (values nil (parser-errors parser))
        (values (ast:make-source :location (scanner:make-source-location) :declarations  decls) nil))))

(defun parse-declaration (parser)
  ;; TODO: find an abstraction for the check-error -> synchronize -> return bad-node sequence
  (let ((loc (scanner:token-location (parser-cur-token parser))))
    (let ((decl (case (scanner:token-type (parser-cur-token parser))
                  (:tok-kw-const (parse-const-declaration parser))
                  (otherwise (error-at-current parser "Expected declaration")))))
      (if (parser-had-error-p parser)
          (progn
            (synchronize! parser)
            (ast:make-bad-declaration :location loc))
          decl))))

(defun parse-const-declaration (parser)
  (let* ((loc (scanner:token-location (parser-cur-token parser)))
         (_const  (consume! parser :tok-kw-const "Expected 'const' before constant declaration"))
         (name (parse-identifier parser))
         (_eql (consume! parser :tok-eql "Expected '=' after constant name"))
         (initializer (parse-const-expression parser)))
    (declare (ignore _const _eql))
    (if (parser-had-error-p parser)
        (ast:make-bad-declaration :location loc)
        (ast:make-const-declaration :location loc :name name :initializer initializer))))

(defun parse-identifier (parser)
  (let ((tok (consume! parser :tok-identifier "Expected identifier")))
    (ast:make-identifier :location (scanner:token-location tok) :name (scanner:token-text tok))))

(defun parse-const-expression (parser)
  ;; we only support literals for now
  (parse-const-literal-expression parser))

(defun parse-const-literal-expression (parser)
  (let* ((tok (parser-cur-token parser))
         (loc (scanner:token-location tok)))
    (case (scanner:token-type tok)
      (:tok-integer
       (advance! parser)
       (ast:make-literal-expression :location loc :token tok))
      (otherwise (error-at-current parser "Expected constant literal")))))

(defun consume! (parser expected-token-type format-string &rest args)
  "Consumes input expecting it to be of the given token type"
  (let ((cur-token (parser-cur-token parser)))
    (if (and cur-token (eql (scanner:token-type cur-token) expected-token-type))
        (prog1 cur-token
          (advance! parser))
        (prog1 (values nil t)
          (apply #'error-at-current parser format-string args)))))

(defun advance! (parser)
  "Read the next legal token. If illegal tokens are encountered along the way the error will be recorded and the parser will continue to advance until a legal token is found."
  (setf (parser-prev-token parser) (parser-cur-token parser))
  (setf (parser-cur-token parser) nil)
  (let ((scanner (parser-scanner parser)))
    (loop for next-tok = (scanner:next-token scanner)
          do
             (setf (parser-prev-token parser) (parser-cur-token parser))
             (setf (parser-cur-token parser) next-tok)
          if (scanner:token-illegal-p next-tok)
            do
               (error-at-current parser "Illegal token ~A" next-tok)
          else
            do (setf (parser-cur-token parser) next-tok)
               (return))
    (values (parser-cur-token parser) (parser-had-error-p parser))))

(defun synchronize! (parser)
  "Attempt to find a synchronization point in the input stream, at which we can attempt to continue parsing.
The parse will likely generate a couple of invalid nodes."
  ;; find next legal token
  (setf (parser-panic-mode-p parser) nil)
  (loop
    (let ((prev-token-type (and (parser-prev-token parser) (scanner:token-type (parser-prev-token parser))))
          (cur-token-type (and (parser-cur-token parser) (scanner:token-type (parser-cur-token parser)))))
      (cond
        ((parser-eof-p parser) (return))
        ((eql prev-token-type :tok-semicolon) (return))
        ((member cur-token-type (list :tok-rbrace :tok-kw-func :tok-kw-if :tok-kw-for)) (return))
        (t (advance! parser))))))

(defun error-at-current (parser format-string &rest args)
  "Record an error at the current location"
  (apply #'error-at parser (parser-cur-token parser) format-string args))

(defun error-at (parser token format-string &rest args)
  "Record an error at the given location. This function signals a continuable parse-error condition."
  (when (parser-panic-mode-p parser)
    (return-from error-at))

  (setf (parser-panic-mode-p parser) t)
  (setf (parser-had-error-p parser) t)

  (let* ((loc (scanner:token-location token))
         (origin (scanner:scan-origin (parser-scanner parser)))
         (parse-error (make-condition 'parse-error-instance :origin origin :location loc :message (apply #'format nil format-string args))))
    (cerror "Continue parsing collecting this error" parse-error)
    (push parse-error (parser-errors parser))))
