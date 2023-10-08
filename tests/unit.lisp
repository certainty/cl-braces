(defpackage :cl-braces/compiler/tests
  (:use #:cl #:parachute))

(in-package :cl-braces/compiler/tests)

(define-test :cl-braces/compiler-suite)

(define-test example-test
  :parent :cl-braces/compiler-suite
  (is = 1 1))
