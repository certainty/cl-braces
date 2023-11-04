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
