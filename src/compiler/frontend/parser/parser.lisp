(in-package :cl-braces.compiler.frontend.parser)

;;; This implements the parser for the highlevel language.
;;; It's a hand rolled recursive descent parser, which collects parse errors and reports them collectively at the end of the parsing process.
;;; When a parse fails, the parse will insert a sentinel node into the AST and continue parsing.
;;; The recovery is relatively simple and attempts to synchronize to the next statement boundary.

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

(defmethod print-object ((error error-detail) stream)
  (print-unreadable-object (error stream :type t)
    (format stream "Line: ~A, Column: ~A => ~A" (location:line (error-location error)) (location:column (error-location error)) (error-message error))))

(defclass state ()
  ((scanner
    :initarg :scanner
    :initform (error "must provide scanner")
    :type parse-buffer
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
   (fail-fast
    :initarg :fail-fast
    :initform nil
    :type boolean
    :documentation "A flag indicating if the parser should fail fast when an error is encountered")
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

(defmethod print-object ((state state) stream)
  (print-unreadable-object (state stream :type t)
    (with-slots (cur-token next-token) state
      (format stream "cur-token: ~A, next-token: ~A" (and cur-token (token:class cur-token)) (and next-token (token:class next-token))))))

;;; ====================================================================================================
;;; Main API for the parser
;;; ====================================================================================================

(-> parse (string &key (:fail-fast boolean) (:production function)) (values (or null ast:node) boolean state &optional))
(defun parse (source-code &key (production #'<statement-list) (fail-fast nil))
  "Parse the given `SOURCE-CODE' and return values (AST, ERRORS, STATE).

   If `FAIL-FAST' is true, the parser will signal an error as soon as it encounters an error.
   If `FAIL-FAST' is false, the parser will attempt to synchronize to the next statement boundary and continue parsing.
   The `PRODUCTION' argument can be used to specify the root production to start parsing "

  (let* ((tokens (scanner:scan-all source-code :fail-fast fail-fast))
         (state (make-instance 'state :scanner (make-parse-buffer tokens) :fail-fast fail-fast)))
    (handler-bind ((error-detail (lambda (c)
                                   (if fail-fast
                                       (invoke-debugger c)
                                       (when (find-restart 'synchronize)
                                         (invoke-restart 'synchronize))))))
      (advance! state)
      (let ((result (funcall production state)))
        (consume! state token:@EOF "Expected end of file")
        (values result (parse-errors state) state)))))

;;; ====================================================================================================
;;; Utility functions to deal with various states of the parser
;;;
;;; Every individual parse has to deal with a couple of base cases
;;; 1. The error cases
;;;     a. The current token is illegal, in which case we signal a parse-error, which will be handled at the top level declaration-parser, which attempts to synchronize to the next statement boundary.
;;;     b. The current token is EOF, in which case we signal an unexpected EOF error.
;;; 2. The parse doesn't apply
;;;    a. The parse is optional, in which case we return nil
;;;    b. The parse is mandatory, in which case we signal an error

(defmacro guard-parse (state &body body)
  `(with-slots (cur-token) ,state
     (when cur-token
       (cond
         ((illegal-token-p ,state)
          (signal-parse-error ,state "Illegal token"))
         (t ,@body)))))

(defun synchronize (state)
  "Synchronize the parser to the next statement boundary."
  (with-slots (cur-token next-token) state
    ;; TODO: clean-up save-points
    (loop
      (cond
        ((eofp state) (return))
        ((token:class-any-p cur-token token:@SEMICOLON token:@RBRACE)
         (advance! state)
         (return-from synchronize))
        ((token:class-any-p next-token token:@IF token:@RETURN)
         (advance! state)
         (return-from synchronize))
        (t (advance! state))))))


;;; parser API
;;; each parser has one of three possible outcomes
;;; 1. it succeeds and returns the parsed node
;;; 2. it figures out it doesn't apply and returns nil
;;; 3. it fails and signals an error
;;;
;;; Parser usee backtracking to deal with the first two cases.
;;; We still try to use backtracking sparringly and only when it's really necessary, because it makes the error reporting more difficult.
;;; We also only introduce the bare minimum of combinators to build new parsers.

(defun try (parser state)
  "The backtracking combinator.
   It executes the given `PARSER' and if it fails it will restore the `STATE' of the `PARSER' to the `STATE' before the `PARSER' was executed.
   It returns the result of the `PARSER' if it succeeds and `NIL' otherwise."
  (guard-parse state
    (with-slots (scanner cur-token next-token) state
      (let ((ctok cur-token)
            (ntok next-token))
        (save-point scanner)
        (a:if-let ((result (funcall parser state))) ; when this exits non-locally via an error, we'll have a save-point hanging
          (prog1 result
            (commit-save-point scanner))
          (prog1 nil
            (rollback-to-save-point scanner)
            (setf cur-token ctok)
            (setf next-token ntok)))))))

(defun expect (parser error-message state)
  "Execute the parser and if it returns `NIL' signal an error with the given message.
   Return the result of the parser otherwise.
  "
  (guard-parse state
    (a:if-let ((result (funcall parser state)))
      result
      (signal-parse-error state error-message))))

(defun skip (parser state)
  "Execute the parser and if it returns `NIL' return `NIL' otherwise return `T'."
  (guard-parse state
    (not (null (funcall parser state)))))

(defun preceded-by (parser1 parser2 state)
  "Execute both `PARSER1' and `PARSER2' and return the result of `PARSER2'."
  (guard-parse state
    (and (funcall parser1 state)
         (try parser2 state))))

(defun followed-by (parser1 parser2 state)
  "Execute both `PARSER1' and `PARSER2' and return the result of `PARSER1'."
  (guard-parse state
    (try (lambda (s)
           (a:when-let ((result (funcall parser1 s)))
             (and (funcall parser2 s) result))))))

;;; ====================================================================================================
;;; Parse implementation for the various language constructs
;;; ====================================================================================================

;;;
;;; SourceFile       = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
;;;
;;; https://golang.org/ref/spec#Source_file
(defun <source-file (state)
  (let ((decls (<top-level-declaration-list state)))
    (accept state 'ast:source-file :declarations decls)))

(defun <top-level-declaration-list (state)
  (guard-parse state
    (let ((decls nil))
      (loop for decl = (<top-level-declaration state)
            when decl
              do (match-any state token:@SEMICOLON)
            while decl
            collect decl))))

;;; Declaration   = ConstDecl | TypeDecl | VarDecl .
;;; TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
;;;
;;; https://golang.org/ref/spec#Declarations_and_scope
;;;
(defun <top-level-declaration (state)
  (restart-case (%<top-level-declaration state)
    (synchronize ()
      :report "Record the error and attempt to resume parsing after the next statement boundary"
      (ignore-errors (synchronize state))
      (return-from <top-level-declaration (accept state 'ast:bad-declaration :message "Expected toplevel declaration")))))

(defun %<top-level-declaration (state)
  (guard-parse state
    (or
     (<function-declaration state)
     (<declaration state))))

;;; Blocks are one of the most used and most basic units in the language
;;;
;;; Block = "{" StatementList "}" .
;;;
;;; Ref: https://golang.org/ref/spec#Blocks
;;; Ref: https://golang.org/ref/spec#Statements


(-> <block (state) (or null ast:block))
(defun <block (state)
  "Parses a block block and returns an `ast:block' node."
  (guard-parse state
    (with-slots (cur-token) state
      (when (token:class= cur-token token:@LBRACE)
        (consume! state token:@LBRACE "Expected block")
        (let ((stmts (<statement-list state)))
          (consume! state token:@RBRACE "Expected '}'")
          (accept state 'ast:block :statements stmts))))))

;;;
;;; StatementList = { Statement ";" } .
;;;
(-> <statement-list (state) (or null ast:statement-list))
(defun <statement-list (state)
  "Parse, a possibly empty, list of statements"
  (guard-parse state
    (let ((stmts nil))
      (loop
        (when (eofp state)
          (return (accept state 'ast:statement-list :statements (nreverse stmts))))
        (a:if-let ((stmt (parse-maybe-terminated #'<statement state))) ; change this to required terminated once the parse is complete
          (push stmt stmts)
          (return (accept state 'ast:statement-list :statements (nreverse stmts))))))))

;;;
;;; Statement =
;;;   Declaration | LabeledStmt | SimpleStmt |
;;;   GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
;;;   FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
;;;   DeferStmt .
;;;
(-> <statement (state) (or null ast:statement))
(defun <statement (state)
  (restart-case (%<statement state)
    (synchronize ()
      :report "Record the error and attempt to resume parsing after the next statement boundary"
      (ignore-errors (synchronize state))
      (return-from <statement (accept state 'ast:bad-statement :message "Expected statement"))))

  )

(-> %<statement (state) (or null ast:node))
(defun %<statement (state)
  (guard-parse state
    (or
     (try #'<declaration state)
     (try #'<if-statement state)
     (try #'<simple-statement state)
     (try #'<block state))))
;;;
;;; IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
;;;
(-> <if-statement (state) (or null ast:if-statement))
(defun <if-statement (state)
  (guard-parse state
    (with-slots (cur-token) state
      (when (token:class= cur-token token:@IF)
        (advance! state)
        ;; committed to parsing an if statement
        (let* ((init (<simple-statement state))
               (condition nil)
               (consequence nil)
               (alternative nil))

          (when (null init)
            (signal-parse-error state "Expected init or condition statement"))

          ;; [init] condition
          (cond
            ((and init (token:class= cur-token token:@SEMICOLON))
             ;; it was an initform, so we consume the token and expect another expression for the condition
             (advance! state)
             (setf condition (expect #'<expression "Expected condition expression" state)))
            (t
             (setf condition init)
             (setf init (make-instance 'ast:empty-statement :location (token:location cur-token)))))

          ;; consequence
          (setf consequence (expect #'<block "Expected consequence block" state))

          ;; [alternative]
          (when (token:class= cur-token token:@ELSE)
            (advance! state)
            (setf alternative (if (token:class= cur-token token:@IF)
                                  (<if-statement state)
                                  (<block state)))
            (when (null alternative)
              (signal-parse-error state "Expected else block")))

          (accept state 'ast:if-statement :init init :condition condition :consequence consequence :alternative alternative))))))

;;;
;;; SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl .
;;;
(-> <simple-statement (state) (or null ast:node))
(defun <simple-statement (state)
  (guard-parse state
    (or
     (try #'<short-variable-declaration state)
     (try #'<assignment state)
     (try #'<expression-statement state))))

;;;
;;; ExpressionStmt = Expression .
;;;
(-> <expression-statement (state) (or null ast:expression-statement))
(defun <expression-statement (state)
  (guard-parse state
    (a:when-let ((expr (<expression state)))
      (accept state 'ast:expression-statement :expression expr))))

;;;
;;; VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
;;; VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
;;;
(defun <variable-declaration (state)
  (guard-parse state
    (with-slots (cur-token) state
      (when (token:class= cur-token token:@VAR)
        (advance! state)
        (let ((var-spec (if (token:class= cur-token token:@LPAREN)
                            (funcall (parse-parenthized-many (lambda (s) (parse-maybe-terminated #'<variable-specification s))) state)
                            (<variable-specification state))))
          (when (null var-spec)
            (signal-parse-error state "Expected variable specification"))
          (accept state 'ast:variable-declaration :specifications (if (consp var-spec) var-spec (list var-spec))))))))

(defun <variable-specification (state)
  (guard-parse state
    (with-slots (cur-token) state
      (let ((identifiers (<identifier-list state))
            (type nil)
            (init nil))
        (when (null identifiers)
          (signal-parse-error state "Expected identifier list"))

        (setf type (try #'<type state))
        (cond
          ((and (null type) (token:class= cur-token token:@EQUAL))
           (advance! state)
           (setf init (<expression-list state))
           (accept state 'ast:variable-specification :identifiers identifiers :initializer init))
          ((and type (not (token:class= cur-token token:@EQUAL)))
           (accept state 'ast:variable-specification :identifiers identifiers :type type))
          ((and type (token:class= cur-token token:@EQUAL))
           (advance! state)
           (setf init (<expression-list state))
           (accept state 'ast:variable-specification :identifiers identifiers :type type :initializer init))
          (t (signal-parse-error state "Expected type or initializer")))))))

;;;
;;; Type      = TypeName [ TypeArgs ] | TypeLit | "(" Type ")" .
;;; TypeName  = identifier | QualifiedIdent .
;;; TypeArgs  = "[" TypeList [ "," ] "]" .
;;; TypeList  = Type { "," Type } .
;;; TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
;;;            SliceType | MapType | ChannelType .
;;;
(defun <type (state)
  ;; we're starting off with the simple case - just an identifier
  (guard-parse state
    (with-slots (cur-token) state
      (when (token:class= cur-token token:@IDENTIFIER)
        (prog1 (accept state 'ast:type-specifier :name cur-token)
          (consume! state token:@IDENTIFIER "Expected identifier"))))))

;;
;;; ShortVarDecl = IdentifierList ":=" ExpressionList .
;;;
(defun <short-variable-declaration (state)
  (guard-parse state
    (with-slots (cur-token next-token) state
      (a:when-let ((lhs (<identifier-list state)))
        (when (token:class= cur-token token:@COLON_EQUAL)
          ;; short variable declaration
          (consume! state token:@COLON_EQUAL "Expected ':=' in short variable declaration")
          (a:if-let ((rhs (<expression-list state)))
            (progn
              (unless (= (length (ast:expression-list-expressions rhs)) (length (ast:identifier-list-identifiers lhs)))
                (signal-parse-error state "Assignment Mismatch. Expected expression list to have the same length as the identifier list"))
              (accept state 'ast:short-variable-declaration :identifiers lhs  :expressions rhs))
            (signal-parse-error state "Expected expression list")))))))

(defun expression-list->identifier-list (state expression-list)
  (let ((expressions (ast:expression-list-expressions expression-list)))
    (unless (every #'(lambda (expr) (eql (type-of expr) 'ast:identifier)) expressions)
      (signal-parse-error state "Expected identifier list"))
    (make-instance 'ast:identifier-list :identifiers expressions :location (ast:location expression-list))))

;;; IdentifierList = identifier { "," identifier } .
(defun <identifier-list (state)
  (guard-parse state
    (a:when-let ((identifier (<identifier state)))
      (let ((identifiers (list identifier)))
        (with-slots (cur-token) state
          (loop
            (cond
              ((token:class= cur-token token:@COMMA)
               (advance! state)
               (push (expect #'<identifier "Expected identifier" state) identifiers))
              (t (return (accept state 'ast:identifier-list :identifiers (nreverse identifiers)))))))))))

;;;
;;; Assignment = ExpressionList assign_op ExpressionList .
;;; assign_op = [ add_op | mul_op ] "=" .
;;;
(-> <assignment (state) (or null ast:assignment-statement))
(defun <assignment (state)
  (guard-parse state
    (with-slots (cur-token) state
      (a:when-let ((lhs (<expression-list state)))
        (when (token:class-any-p cur-token token:@EQUAL token:@MUL_EQUAL token:@PLUS_EQUAL)
          (let ((operator cur-token))
            (advance! state)
            (a:if-let ((rhs (<expression-list state)))
              (accept state 'ast:assignment-statement :lhs lhs :operator operator :rhs rhs)
              (signal-parse-error state "Expected expression list"))))))))

;;;
(defun <declaration (state)
  (guard-parse state
    (or
     (<variable-declaration state))))

;;;
;;; FunctionDecl = "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ] .
;;; FunctionName = identifier .
;;; FunctionBody = Block .
;;;
;;; https://golang.org/ref/spec#Function_declarations
;;; https://golang.org/ref/spec#Function_types

;;; For now without support for generics
(-> <function-declaration (state) (or null ast:function-declaration))
(defun <function-declaration (state)
  (guard-parse state
    (with-slots (cur-token) state
      (when (token:class= cur-token token:@FUNC)
        (advance! state)
        (let ((name (expect #'<identifier "Expected identifier" state))
              (signature (expect #'<function-signature "Expected function signature" state))
              (body (expect #'<block state "Expected function block")))
          (accept state 'ast:function-declaration :name name :signature signature :body body))))))

;;;
;;; Signature      = Parameters [ Result ] .
;;; Result         = Parameters | Type .
;;; Parameters     = "(" [ ParameterList [ "," ] ] ")" .
;;; ParameterList  = ParameterDecl { "," ParameterDecl } .
;;; ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
;;;
;;; https://golang.org/ref/spec#Function_types
;;;
(-> <function-signature (state) (or null ast:function-signature))
(defun <function-signature (state)
  (let ((parameter-decls (expect #'<function-parameters "Expected function parameters" state))
        (return-parameters (try #'<function-return-paramters state))
        (return-type (try #'<type state)))
    (accept state 'ast:function-signature :parameters parameter-decls :returne-type return-type :return-parameters return-parameters)))

(-> <function-parameters (state) (or null list))
(defun <function-parameters (state)
  (guard-parse state
    (with-slots (cur-token) state
      (when (token:class= cur-token token:@LPAREN)
        (advance! state)
        (prog1 (<function-parameter-list state)
          (match-any state token:@COMMA) ; trailing comma
          (consume! state token:@RPAREN "Expected ')' after function parameters"))))))

(-> <function-parameter-list (state) (or null list))
(defun <function-parameter-list (state)
  (guard-parse state
    (a:when-let ((first-parameter (<parameter-declaration state)))
      (let ((parameters (list first-parameter)))
        (with-slots (cur-token) state
          (loop
            (a:if-let ((param (followed-by #'<comma #'<parameter-declaration state)))
              (push param parameters)
              (return (nreverse parameters)))))))))

(-> <parameter-declaration (state) (or null ast:parameter-declaration))
(defun <parameter-declaration (state)
  (guard-parse state
    (let ((identifiers (try #'<identifier-list state))
          (splat (try #'<splat-parameter state))
          (type (expect #'<type "Expected paramter type" state) ))
      (accept state 'ast:parameter-declaration :identifiers identifiers :splat splat :type type))))

(defun <splat-parameter (state)
  (guard-parse state
    (with-slots (cur-token) state
      (when (token:class= cur-token token:@ELLIPSIS)
        (advance! state)
        (accept state 'ast:parameter-splat :token cur-token)))))

;;;
;;; ExpressionList = Expression { "," Expression } .
;;;
(defun <expression-list (state)
  (guard-parse state
    (a:when-let ((expression (<expression state)))
      (let ((expresions (list expression)))
        (with-slots (cur-token) state
          (loop
            (cond
              ((token:class= cur-token token:@COMMA)
               (advance! state)
               (push (expect #'<expression "Expected expression" state) expresions))
              (t (return (accept state 'ast:expression-list :expressions (nreverse expresions)))))))))))

(defun parse-parenthized-many (parser &key (open-paren token:@LPAREN) (close-paren token:@RPAREN))
  "Parses first the `OPEN-PAREN', then the provided `PARSER' repeatedly until the `CLOSE-PAREN' token is encountered."
  (lambda (state)
    (guard-parse state
      (with-slots (cur-token) state
        (when (token:class= cur-token open-paren)
          (advance! state)
          (let ((result nil))
            (loop
              (when (eofp state)
                (signal-parse-error state "Unexpected EOF"))
              (when (token:class= cur-token close-paren)
                (advance! state)
                (return (nreverse result)))
              (a:when-let ((next (funcall parser state)))
                (push next result)))))))))

(defun parse-terminated (parser state)
  (guard-parse state
    (a:when-let ((result (funcall parser state)))
      (consume! state token:@SEMICOLON "Expected ';' after statement")
      result)))

(defun parse-maybe-terminated (parser state)
  (guard-parse state
    (a:when-let ((result (funcall parser state)))
      (match-any state token:@SEMICOLON)
      result)))

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

(a:define-constant +operator-rules+
    (s:dict
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

(defun <expression (state &optional (current-min-precedence +precedence-assignment+))
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
 (<expression state +precedence-assignment+)
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
                   (setf right (<expression state next-min-precedence))
                   (setf left (accept state 'ast:binary-expression :lhs left :operator operator :rhs right))))))))

(defmacro when-token (state expected-class &body body)
  `(with-slots (cur-token) ,state
     (when (and cur-token (token:class= cur-token ,expected-class))
       ,@body)))

(defun parse-primary-expression (state)
  "Parse a primary expression, which are usually terminal nodes. The prefix `primary-' is often used in parsers to denot these types of productions."
  (guard-parse state
    (or
     (try #'parse-literal state)
     (try #'parse-unary-expression state)
     (try #'<identifier state)
     (try #'parse-grouping-expression state))))

(defun <identifier (state)
  (guard-parse state
    (with-slots (cur-token) state
      (when (token:class= cur-token token:@IDENTIFIER)
        (let ((tok (consume! state token:@IDENTIFIER "Expected identifier")))
          (accept state 'ast:identifier :token tok))))))

(defun <comma (state)
  (guard-parse state
    (when-token state token:@COMMA
      (advance! state)
      (accept state 'ast:comma :token cur-token))))

(defun parse-literal (state)
  "Recognizes a literal expression"
  (guard-parse state
    (or
     (try #'parse-boolean-literal state)
     (try #'parse-number-literal state))))

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
        (accept state 'ast:literal :token tok)))))

(defun parse-grouping-expression (state)
  (when-token state token:@LPAREN
    (let ((tok (consume! state token:@LPAREN "Expected '('")))
      (when tok
        (unless (token:class= tok token:@ILLEGAL)
          (let ((expr (<expression state +precedence-term+)))
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
        (support:unreachable! "No current token"))))

(-> advance! (state) (values token:token token:token))
(defun advance! (state)
  (with-slots (next-token cur-token scanner) state
    (cond
      ((null cur-token)
       (setf cur-token (read-token scanner))
       (setf next-token (read-token scanner)))
      ((null next-token)
       (setf next-token (read-token scanner)))
      (t (setf cur-token next-token)
         (setf next-token (read-token scanner))))
    (when (token:class= cur-token token:@EOF)
      (setf next-token cur-token))
    (values cur-token next-token)))

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
  (let ((all-classes (cons token-class other-token-classes)))
    (dolist (next-class all-classes)
      (when-token state next-class
        (advance! state)
        (return next-class)))))

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
    (let* ((span (span:for token))
           (parse-error (make-condition 'error-detail :location (span:from span) :message (apply #'format nil format-string args))))
      (push parse-error errors)
      (error parse-error))))
