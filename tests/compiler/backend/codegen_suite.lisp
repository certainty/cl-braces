(in-package :tests.backend.codegen)

(define-test codegen-grouping-expression ()
  (let ((chunk (compiler:compile-this  "(1 + 3)")))
    (with-slots (code constants) chunk
      (assert-equal 3 (length code))
      (assert-equal 2 (length constants))
      (snapshots:assert-snapshot-equals "grouping-expression.snapshot" code))))

(define-test codegen-unary-expression ()
  (let ((chunk (compiler:compile-this "-1")))
    (let ((code (bytecode::chunk-code chunk))
          (constants (bytecode::chunk-constants chunk)))
      (assert-equal 2 (length code))
      (assert-equal 1 (length constants))
      (snapshots:assert-snapshot-equals "unary-expression.snapshot" code))))

(define-test codegen-binary-expression ()
  (let ((chunk (compiler:compile-this "1 + 3")))
    (let ((code (bytecode::chunk-code chunk))
          (constants (bytecode::chunk-constants chunk)))
      (assert-equal 3 (length code))
      (assert-equal 2 (length constants))
      (snapshots:assert-snapshot-equals "binary-expression.snapshot" code))))
