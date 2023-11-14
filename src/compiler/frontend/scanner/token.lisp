(in-package :cl-braces.compiler.frontend.token)

(serapeum:defunion token-class
  @ILLEGAL
  @EOF
  @LPAREN
  @RPAREN
  @INTEGER
  @PLUS
  @MINUS
  @STAR
  @SLASH
  @SEMICOLON
  @COLON_EQUAL
  @IDENTIFIER)

(defclass token ()
  ((class :reader class
          :initarg :class
          :initform (error "no type given")
          :type token-class)
   (lexeme :reader lexeme
           :initarg :lexeme
           :initform (error "no lexeme given")
           :type string)
   (value :reader value
          :initarg :value
          :initform nil
          :type (or null t))
   (location :reader location
             :initarg :location
             :initform (error "no location given")
             :type location:source-location))
  (:documentation "A token is a single unit of input. It is the smallest unit of input that the parser can work with."))

(defmethod print-object ((token token) stream)
  (with-slots (class lexeme value location) token
    (print-unreadable-object (token stream :type t :identity t)
      (format stream "class: ~a lexeme: ~a value: ~a location: ~a" class lexeme value location))))

(-> class= (token token-class) boolean)
(defun class= (token expected-class)
  "Returns true if the token's class is equal to the given `expected-class.'"
  (with-slots (class) token
    (eql class expected-class)))

(-> literal-p (token) boolean)
(defun literal-p (token)
  "Returns true if the token is a literal."
  (or (class= token @INTEGER)))
