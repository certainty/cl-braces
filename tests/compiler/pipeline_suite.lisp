(in-package :tests.compiler.pipeline)

(define-test smoke-test ()
  "Makes sure the pipeline runs and doesn't crash."
  (assert-true (bytecode::chunk-p (compiler:compile-this "3+3")))
  (assert-true (bytecode::chunk-p (compiler:compile-this "(3+3)*3"))))
