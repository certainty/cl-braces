(in-package :tests.compiler.pipeline)

(define-test smoke-test ()
  "Makes sure the pipeline runs and doesn't crash."
  (let ((chunk (compiler:compile-this "3+4")))
    (assert-true (bytecode::chunk-p chunk))))
