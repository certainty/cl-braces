(in-package :tests.frontend.ast)

(define-test span-unary-expression ()
  (let* ((expr (parser:parse "-3"))
         (span (location:span-for expr)))
    (assert-equal 1 (location:line (location:span-from span)))
    (assert-equal 1 (location:column (location:span-from span)))

    (assert-equal 1 (location:line (location:span-to span)))
    (assert-equal 2 (location:column (location:span-to span)))))

(define-test span-binary-expression ()
  (let* ((expr (parser:parse "4-3"))
         (span (ast:span expr)))
    (assert-equal 1 (location:line (location:span-from span)))
    (assert-equal 1 (location:column (location:span-from span)))

    (assert-equal 1 (location:line (location:span-to span)))
    (assert-equal 3 (location:column (location:span-to span)))))
