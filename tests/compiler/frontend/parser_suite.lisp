(in-package :tests.frontend.parser)

(define-test parse-integer-literal ()
  "Parse an integer literal"
  (let* ((program (parser:parse "3"))
         (node (car (ast:program-declarations program))))
    (assert-equal 'ast:literal (type-of node))
    (assert-equal 3 (ast:literal-value node))))

(define-test parse-unary-minus ()
  "Parse simple unary minus"
  (let* ((program (parser:parse "-3"))
         (node (car (ast:program-declarations program))))
    (assert-eql 'ast:unary-expression (type-of node))
    (assert-eql token:@MINUS (token:class (ast:unary-expression-operator node)))
    (assert-eql 'ast:literal (type-of (ast:unary-expression-operand node)))))

(define-test parse-unary-plus ()
  "Parse simple unary plus"
  (let* ((program (parser:parse "+3"))
         (node (car (ast:program-declarations program))))
    (assert-eql 'ast:unary-expression (type-of node))
    (assert-eql token:@PLUS (token:class (ast:unary-expression-operator node)))
    (assert-eql 'ast:literal (type-of (ast:unary-expression-operand node)))))

(define-test parse-grouping ()
  "Parse grouping expression with two operands"
  (let* ((program (parser:parse "(3 - 4)"))
         (node (car (ast:program-declarations program))))
    (assert-eql 'ast:grouping-expression (type-of node))
    (assert-eql 'ast:binary-expression (type-of (ast:grouping-expression-expression node)))))

(define-test parse-binary-plus ()
  "Parse binary plus with two operands"
  (let* ((program (parser:parse "3 + 4"))
         (node (car (ast:program-declarations program))))
    (assert-eql 'ast:binary-expression (type-of node))
    (assert-eql 'ast:literal (type-of (ast:binary-expression-lhs node)))
    (assert-eql token:@PLUS (token:class (ast:binary-expression-operator node)))
    (assert-eql 'ast:literal (type-of (ast:binary-expression-rhs node)))))

(define-test parse-binary-minus ()
  "Parse binary minus with two operands"
  (let* ((program (parser:parse "3 - 4"))
         (node (car (ast:program-declarations program))))
    (assert-eql 'ast:binary-expression (type-of node))
    (assert-eql 'ast:literal (type-of (ast:binary-expression-lhs node)))
    (assert-eql token:@MINUS (token:class (ast:binary-expression-operator node)))
    (assert-eql 'ast:literal (type-of (ast:binary-expression-rhs node)))))

(define-test parse-binary-with-mixed-precedence-operators ()
  "Parse binary expressions with operators with mixed precedence"
  (let* ((program (parser:parse "3 - 4 * 5"))
         (node (car (ast:program-declarations program))))
    (assert-eql 'ast:binary-expression (type-of node))
    (assert-eql token:@MINUS (token:class (ast:binary-expression-operator node)))

    (let ((lhs (ast:binary-expression-lhs node))
          (rhs (ast:binary-expression-rhs node)))
      (assert-eql 'ast:literal (type-of lhs))
      (assert-eql 'ast:binary-expression (type-of rhs))

      (let ((rhs-lhs (ast:binary-expression-lhs rhs))
            (rhs-rhs (ast:binary-expression-rhs rhs)))
        (assert-eql 'ast:literal (type-of rhs-lhs))
        (assert-eql token:@STAR (token:class (ast:binary-expression-operator rhs)))
        (assert-eql 'ast:literal (type-of rhs-rhs))))))

(define-test parse-binary-with-explicit-grouping ()
  "Parse a binary expression that has explicit grouping"
  (let* ((program (parser:parse "(3 - 4) * 5"))
         (node (car (ast:program-declarations program))))

    (assert-eql 'ast:binary-expression (type-of node))
    (assert-eql token:@STAR (token:class (ast:binary-expression-operator node)))
    (let ((lhs (ast:binary-expression-lhs node))
          (rhs (ast:binary-expression-rhs node)))
      (assert-eql 'ast:grouping-expression (type-of lhs))
      (assert-eql 'ast:literal (type-of rhs))

      (let ((inner (ast:grouping-expression-expression lhs)))
        (assert-eql 'ast:binary-expression (type-of inner))
        (assert-eql token:@MINUS (token:class (ast:binary-expression-operator inner)))
        (assert-eql 'ast:literal (type-of (ast:binary-expression-lhs inner)))
        (assert-eql 'ast:literal (type-of (ast:binary-expression-rhs inner)))))))

(define-test parse-reprodcue-bug ()
  (multiple-value-bind (ast had-errors) (parser:parse "(3 + 3) * 3")
    (declare (ignore ast))
    (assert-false had-errors)))

(define-test parse-short-assignment ()
  (multiple-value-bind (ast had-errors state) (parser:parse "a := 3")
    (assert-false had-errors)))
