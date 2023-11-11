(in-package :tests.backend.codegen)

(define-test codegen-grouping-expression ()
  (let ((chunk (codegen:generate-chunk (parser:parse "(1 + 3)"))))
    (let ((code (bytecode::chunk-code chunk))
          (constants (bytecode::chunk-constants chunk)))
      (assert-equal 3 (length code))
      (assert-equal 2 (length constants))
      (snapshots:assert-snapshot-equals "grouping-expression.snapshot" code))))
