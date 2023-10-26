(in-package :cl-braces.compiler.tests)

(define-test :cl-braces/compiler/frontend/scanner-suite
  :parent :cl-braces/compiler-suite)

(defun scans-as (input)
  (let ((tok (next-token (string->scanner input))))
    (values (token-type tok) (token-text tok) (token-value tok))))

(define-test skip-whitespaces
  :parent :cl-braces/compiler/frontend/scanner-suite
  (let ((comments (next-token (string->scanner (format nil "// foo bar~%valid"))))
        (newlines (next-token (string->scanner (format nil "   ~% bar")))))
    (is string= (token-text comments) "valid")
    (is string= (token-text newlines) "bar")))

(define-test next-token-smoke-test
  :parent :cl-braces/compiler/frontend/scanner-suite
  (let* ((s (string->scanner (format nil "// this is a comment ~% func main(){ return 3 }")))
         (tokens (scan-all s)))
    (is = 9 (length tokens))
    (is equal (mapcar #'token-type tokens) (list :tok-kw-func :tok-identifier :tok-lparen :tok-rparen :tok-lbrace :tok-kw-return :tok-integer :tok-rbrace :tok-eof))))

(define-test scan-identifier
  :parent :cl-braces/compiler/frontend/scanner-suite
  (is-values (scans-as "foo") (eql :tok-identifier) (string= "foo") (eql nil))
  (is-values (scans-as "funcFoo") (eql :tok-identifier) (string= "funcFoo") (eql nil))
  (is-values (scans-as "foo234_something") (eql :tok-identifier) (string= "foo234_something") (eql nil))
  (is-values (scans-as "someSnakeCase1234") (eql :tok-identifier) (string= "someSnakeCase1234") (eql nil)))

(define-test scan-integer
  :parent :cl-braces/compiler/frontend/scanner-suite
  (is-values (scans-as "12345") (eql :tok-integer) (string= "12345") (= 12345))
  (is-values (scans-as "+12") (eql :tok-integer) (string= "+12") (= 12))
  (is-values (scans-as "-12") (eql :tok-integer) (string= "-12") (= -12)))

(define-test scan-keyword
  :parent :cl-braces/compiler/frontend/scanner-suite
  (is eql :tok-kw-func (scans-as "func"))
  (is eql :tok-kw-return (scans-as "return"))
  (is eql :tok-kw-if (scans-as "if"))
  (is eql :tok-kw-else (scans-as "else"))
  (is eql :tok-kw-switch (scans-as "switch"))
  (is eql :tok-kw-case (scans-as "case"))
  (is eql :tok-kw-default (scans-as "default"))
  (is eql :tok-kw-for (scans-as "for"))
  (is eql :tok-kw-range (scans-as "range")))

(define-test scan-simple
  :parent :cl-braces/compiler/frontend/scanner-suite
  (is eql :tok-lbrace (scans-as "{"))
  (is eql :tok-rbrace (scans-as "}"))
  (is eql :tok-lbracket (scans-as "["))
  (is eql :tok-rbracket (scans-as "]"))
  (is eql :tok-lparen (scans-as "("))
  (is eql :tok-rparen (scans-as ")"))
  (is eql :tok-dot (scans-as "."))
  (is eql :tok-comma (scans-as ","))
  (is eql :tok-colon (scans-as ":"))
  (is eql :tok-semicolon (scans-as ";"))
  (is eql :tok-eql (scans-as "="))
  (is eql :tok-bang (scans-as "!"))
  (is eql :tok-op-double-eql (scans-as "=="))
  (is eql :tok-op-bang-eql (scans-as "!="))

  (is eql :tok-op-dec (scans-as "--"))
  (is eql :tok-op-inc (scans-as "++"))
  (is eql :tok-op-colon-eql (scans-as ":="))
  (is eql :tok-asterisk (scans-as "*"))
  (is eql :tok-ampersand (scans-as "&")))

(define-test scan-with-illegal-token
  :parent :cl-braces/compiler/frontend/scanner-suite
  (let ((all-tokens (scan-all (string->scanner "const i _< 3"))))
    (is eql (list :tok-kw-const :tok-identifier :tok-identifier :tok-illegal :tok-integer :tok-eof) (mapcar #'token-type all-tokens))))
