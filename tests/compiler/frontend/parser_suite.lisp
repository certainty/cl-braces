(in-package :cl-braces.compiler.tests)

(defsuite parser-suite (frontend-suite))

(deftest parse-integer-literal (parser-suite)
  "Parse an integer literal"
  (let ((node (parser:parse "3")))
    (assert-equalp (type-of node) 'ast:literal)
    (assert-equalp (ast:literal-value node) 3)))

(deftest parse-unary-minus (parser-suite)
  "Parse simple unary minus"
  (let ((node (parser:parse "-3")))
    (assert-equalp (type-of node) 'ast:unary-expression)
    (assert-equalp (token:class (ast:unary-expression-operator node)) token:@MINUS)
    (assert-equalp (type-of (ast:unary-expression-operand node)) 'ast:literal)))

(deftest parse-unary-plus (parser-suite)
  "Parse simple unary plus"
  (let ((node (parser:parse "+3")))
    (assert-equalp (type-of node) 'ast:unary-expression)
    (assert-equalp (token:class (ast:unary-expression-operator node)) token:@PLUS)
    (assert-equalp (type-of (ast:unary-expression-operand node)) 'ast:literal)))
