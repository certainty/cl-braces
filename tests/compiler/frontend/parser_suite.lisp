(in-package :tests.frontend.parser)


(define-test parse-integer-literal ()
  "Parse an integer literal"
  (let ((node (parser:parse "3")))
    (assert-equal 'ast:literal (type-of node))
    (assert-equal 3 (ast:literal-value node))))

(define-test parse-unary-minus ()
  "Parse simple unary minus"
  (let ((node (parser:parse "-3")))
    (assert-eql 'ast:unary-expression (type-of node))
    (assert-eql token:@MINUS (token:class (ast:unary-expression-operator node)))
    (assert-eql 'ast:literal (type-of (ast:unary-expression-operand node)))))

(define-test parse-unary-plus ()
  "Parse simple unary plus"
  (let ((node (parser:parse "+3")))
    (assert-eql 'ast:unary-expression (type-of node))
    (assert-eql token:@PLUS (token:class (ast:unary-expression-operator node)))
    (assert-eql 'ast:literal (type-of (ast:unary-expression-operand node)))))

(define-test parse-binary-plus ()
  "Parse binary plus with two operands"
  (let ((node (parser:parse "3 + 4")))
    (assert-eql 'ast:binary-expression (type-of node))
    (assert-eql 'ast:literal (type-of (ast:binary-expression-lhs node)))
    (assert-eql token:@PLUS (token:class (ast:binary-expression-operator node)))
    (assert-eql 'ast:literal (type-of (ast:binary-expression-rhs node)))))
