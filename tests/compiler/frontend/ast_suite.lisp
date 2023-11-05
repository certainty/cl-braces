(in-package :tests.frontend.ast)

(define-test span-unary-expression ()
  (let* ((expr (parser:parse "-3"))
         (span (ast:span expr)))
    (assert-equal 1 (token:location-line (ast:span-from span)))
    (assert-equal 1 (token:location-column (ast:span-from span)))

    (assert-equal 1 (token:location-line (ast:span-to span)))
    (assert-equal 2 (token:location-column (ast:span-to span)))))

(define-test span-binary-expression ()
  (let* ((expr (parser:parse "4-3"))
         (span (ast:span expr)))
    (assert-equal 1 (token:location-line (ast:span-from span)))
    (assert-equal 1 (token:location-column (ast:span-from span)))

    (assert-equal 1 (token:location-line (ast:span-to span)))
    (assert-equal 3 (token:location-column (ast:span-to span)))))
