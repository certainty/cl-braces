(in-package :cl-braces/compiler/frontend/hr/parser)

(defstruct (span (:conc-name span-) (:constructor make-span (provided-from provided-to)))
  (from provided-from :type source-location :read-only t)
  (to provided-to :type source-location :read-only t))

(defstruct (braces-parse-error (:conc-name error-) (:constructor make-parse-error (origin provided-span provided-what)))
  (where origin :type source-origin :read-only t)
  (span provided-span :type span :read-only t)
  (what provided-what :type string :read-only t))


(defstruct (parse-state (:conc-name parser-) (:constructor make-parser (provided-scanner)))
  (scanner provided-scanner :type scan-state)
  (prev-token nil :type (or null token))
  (cur-token nil :type (or null token))
  (errors nil)
  (had-error nil :type boolean)
  (panic-mode-p nil :type boolean))

(defclass ast-node ()
  ((id :initform (error "must provide node-id") :initarg :node-id :accessor ast-node-id)
   (span :initform (error "must provide span") :initarg :span :reader ast-node-span)))

(defclass ast-expression (ast-node) ())

(defclass ast-bad-expression (ast-expression) ())

(defclass ast-literal-expression (ast-expression)
  ((token :initarg :token :reader ast-literal-expression-token)))

(defclass ast-identifier (ast-expression)
  ((name :initarg :name :reader ast-identifier)))

(defclass ast-statement (ast-node) ())

(defclass ast-bad-statement (ast-statement) ())

(defclass ast-declaration (ast-node) ())

(defclass ast-bad-declaration (ast-declaration) ())

(defclass ast-source (ast-node)
  ((declarations :initarg :declarations :reader ast-source-declarations)))

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

  (let* ((span (make-span (token-location token) (token-location token)))
         (origin (scan-origin (parser-scanner parser))))
    (push (make-parse-error origin span (apply #'format nil format-string args))
          (parser-errors parser))))
