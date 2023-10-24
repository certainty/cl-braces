(in-package :cl-braces/compiler/frontend/hr/parser)

(defstruct (braces-parse-error (:conc-name error-) (:constructor make-parse-error (provided-origin provided-location provided-what)))
  (origin provide-origin :type source-origin :read-only t)
  (location provided-location :type location :read-only t)
  (what provided-what :type string :read-only t))

(defstruct (parse-state (:conc-name parser-) (:constructor make-parser (provided-scanner)))
  (scanner provided-scanner :type scan-state)
  (prev-token nil :type (or null token))
  (cur-token nil :type (or null token))
  (errors nil)
  (had-error nil :type boolean)
  (panic-mode-p nil :type boolean))

(defstruct (ast-node (:conc-name ast-node-))
  (id (error "must provide node-id") :type symbol :read-only t)
  (location (error "must provide location") :type source-location :read-only t))

(defstruct (ast-expression (:include ast-node)))

(defstruct (ast-bad-expression (:include ast-node)))

(defstruct (ast-literal-expression (:conc-name ast-literal-exp-) (:include ast-expression))
  (token (error "must provide token") :type token :read-only t))

(defstruct (ast-identifier (:conc-name ast-identifier-) (:include ast-expression))
  (name (error "must provide token") :type string :read-only t))

(defstruct (ast-statement (:include ast-node)))

(defstruct (ast-bad-statement (:include ast-statement)))

(defstruct (ast-declaration (:include ast-node)))

(defstruct (ast-bad-declaration (:include ast-declaration)))

(defstruct (ast-source (:conc-name ast-source-) (:include ast-node))
  (declarations (error "must provide declarations") :type list :read-only t))

(defun string->parser (input)
  (make-parser (string->scanner input)))

(-> parse (parse-state) (values (or null ast-source) list))
(defun parse (parser)
  "Parse the input and return the AST"
  (advance! parser)
  (if (parser-had-error parser)
      (values nil (parser-errors parser))
      (values nil nil)))

(defun consume! (parser expected-token-type format-string &rest args)
  "Consumes input expecting it to be of the given token type"
  (let ((tok (advance! parser)))
    (unless (eql (token-type tok) expected-token-type)
      (apply #'error-at parser tok format-string args))))

(defun advance! (parser)
  "Read the next legal token. If illegal tokens are encountered along the way the error will be recorded and the parser will continue to advance until a legal token is found."
  (rotatef (parser-prev-token parser) (parser-cur-token parser))
  (let ((scanner (parser-scanner parser)))
    (loop for next-tok = (next-token scanner)
          if (token-illegal-p next-tok)
            do (error-at-current scanner "Illegal token ~A" next-tok)
          else
            do (setf (parser-cur-token parser) next-tok)
               (return))
    (parser-cur-token parser)))

(defun error-at-current (parser format-string &rest args)
  "Record an error at the current location"
  (apply #'error-at (parser-cur-token parser) format-string args))

(defun error-at (parser token format-string &rest args)
  (when (parser-panic-mode-p parser)
    (return-from error-at))

  (setf (parser-panic-mode-p parser) t)
  (setf (parser-had-error parser) t)

  (let* ((loc (token-location token))
         (origin (scan-origin (parser-scanner parser))))
    (push (make-parse-error origin loc (apply #'format nil format-string args))
          (parser-errors parser))))
