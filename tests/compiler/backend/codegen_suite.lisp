(in-package :tests.backend.codegen)

(define-test codegen-grouping-expression ()
  (let ((chunk (compiler:compile  "(1 + 3)")))
    (assert-equal 6 (length (bytecode:chunk-code chunk)))
    (assert-equal 3 (length (bytecode:chunk-constants chunk)))
    (snapshots:assert-snapshot-equals "grouping-expression.snapshot" (bytecode:chunk-code chunk))))

(define-test codegen-unary-expression ()
  (let ((chunk (compiler:compile "-1")))
    (assert-equal 5 (length (bytecode:chunk-code chunk)))
    (assert-equal 2 (length (bytecode:chunk-constants chunk)))
    (snapshots:assert-snapshot-equals "unary-expression.snapshot" (bytecode:chunk-code chunk))))

(define-test codegen-binary-expression ()
  (let ((chunk (compiler:compile "1 + 3")))
    (assert-equal 6 (length (bytecode:chunk-code chunk)))
    (assert-equal 3 (length (bytecode:chunk-constants chunk)))
    (snapshots:assert-snapshot-equals "binary-expression.snapshot" (bytecode:chunk-code chunk))))
