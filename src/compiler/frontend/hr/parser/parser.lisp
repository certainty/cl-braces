(in-package :cl-braces.compiler.frontend.parser)

(defstruct (braces-parse-error (:conc-name error-) (:constructor make-parse-error (provided-origin provided-location provided-what)))
  (origin provided-origin :type scanner:source-origin :read-only t)
  (location provided-location :type scanner:source-location :read-only t)
  (what provided-what :type string :read-only t))

(defstruct (parse-state (:conc-name parser-) (:constructor make-parser (provided-scanner)))
  (scanner provided-scanner :type scanner:scan-state)
  (prev-token nil :type (or null scanner:token))
  (cur-token nil :type (or null scanner:token))
  (errors nil)
  (had-error-p nil :type boolean)
  (panic-mode-p nil :type boolean))

(defparameter *ast-node-id-counter* 1)
(defparameter *fail-fast* t)

(defun next-ast-node-id ()
  (prog1 *ast-node-id-counter*
    (incf *ast-node-id-counter*)))

(defstruct (ast-node (:conc-name ast-node-))
  (id (next-ast-node-id) :type positive-fixnum :read-only t)
  (location (error "must provide location") :type scanner:source-location :read-only t))

(defstruct (ast-expression (:include ast-node)))

(defstruct (ast-bad-expression (:include ast-node)))

(defstruct (ast-literal-expression (:conc-name ast-literal-exp-) (:include ast-expression))
  (token (error "must provide token") :type scanner:token :read-only t))

(defstruct (ast-identifier (:conc-name ast-identifier-) (:include ast-expression))
  (name (error "must provide token") :type string :read-only t))

(defstruct (ast-statement (:include ast-node)))

(defstruct (ast-bad-statement (:include ast-statement)))

(defstruct (ast-declaration (:include ast-node)))

(defstruct (ast-const-declaration (:conc-name ast-const-decl-) (:include ast-declaration))
  (name (error "must provide name") :type ast-identifier :read-only t)
  (initializer (error "must provide initializer") :type ast-literal-expression :read-only t))

(defstruct (ast-bad-declaration (:include ast-declaration)))

(defstruct (ast-source (:conc-name ast-source-) (:include ast-node))
  (declarations (error "must provide declarations") :type list :read-only t))

(defun string->parser (input)
  (make-parser (scanner:string->scanner input)))

(defun parser-eof-p (parser)
  (scanner:token-eof-p (parser-cur-token parser)))

(-> parse (parse-state) (values (or null ast-source) list))
(defun parse (parser)
  (setf *ast-node-id-counter* 1)
  (parse-source parser))

(-> parse-source (parse-state) (values (or null ast-source) list))
(defun parse-source (parser)
  "Parse the input and return the AST"
  (advance! parser)
  (let ((decls (loop for decl = (parse-declaration parser)
                     collect decl
                     until (or (and *fail-fast* (parser-had-error-p parser)) (parser-eof-p parser)))))
    (if (parser-had-error-p parser)
        (values nil (parser-errors parser))
        (values (make-ast-source :location (scanner:make-source-location) :declarations  decls) nil))))

(defun parse-declaration (parser)
  ;; TODO: replace with ecase-of
  (let ((loc (scanner:token-location (parser-cur-token parser))))
    (case (scanner:token-type (parser-cur-token parser))
      (:tok-kw-const (parse-const-declaration parser))
      (otherwise
       (make-ast-bad-declaration :location loc)))))

(defun parse-const-declaration (parser)
  (let* ((loc (scanner:token-location (parser-cur-token parser)))
         (_const  (consume! parser :tok-kw-const "Expected 'const' before constant declaration"))
         (name (parse-identifier parser))
         (_eql (consume! parser :tok-eql "Expected '=' after constant name"))
         (initializer (parse-const-expression parser)))
    (declare (ignore _const _eql))
    (if (parser-had-error-p parser)
        (make-ast-bad-declaration :location loc)
        (make-ast-const-declaration :location loc :name name :initializer initializer))))

(defun parse-identifier (parser)
  (let ((tok (consume! parser :tok-identifier "Expected identifier")))
    (make-ast-identifier :location (scanner:token-location tok) :name (scanner:token-text tok))))

(defun parse-const-expression (parser)
  ;; we only support literals for now
  (parse-const-literal-expression parser))

(defun parse-const-literal-expression (parser)
  (let* ((tok (parser-cur-token parser))
         (loc (scanner:token-location tok)))
    (case (scanner:token-type tok)
      (:tok-integer
       (advance! parser)
       (make-ast-literal-expression :location loc :token tok))
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
  (rotatef (parser-prev-token parser) (parser-cur-token parser))
  (let ((scanner (parser-scanner parser)))
    (loop for next-tok = (scanner:next-token scanner)
          if (scanner:token-illegal-p next-tok)
            do (error-at-current scanner "Illegal token ~A" next-tok)
          else
            do (setf (parser-cur-token parser) next-tok)
               (return))
    (values (parser-cur-token parser) (parser-had-error-p parser))))

(defun error-at-current (parser format-string &rest args)
  "Record an error at the current location"
  (apply #'error-at parser (parser-cur-token parser) format-string args))

(defun error-at (parser token format-string &rest args)
  (when (parser-panic-mode-p parser)
    (return-from error-at))

  (setf (parser-panic-mode-p parser) t)
  (setf (parser-had-error-p parser) t)

  (let* ((loc (scanner:token-location token))
         (origin (scanner:scan-origin (parser-scanner parser))))
    (push (make-parse-error origin loc (apply #'format nil format-string args)) (parser-errors parser))))
