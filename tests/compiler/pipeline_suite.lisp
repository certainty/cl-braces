(in-package :tests.compiler.pipeline)

(define-test smoke-test ()
  "Makes sure the pipeline runs and doesn't crash."
  (assert (compiler:compile-string "3+3"))
  (assert (compiler:compile-string "(3+3)*3")))
