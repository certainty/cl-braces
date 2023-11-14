(in-package :cl-braces.tests.system)

(defun compile-and-run (code)
  (value:unbox (machine:execute (compiler:compile-this code))))

(define-test global-variables ()
  (assert-equal
   10
   (compile-and-run
    "x := 3
     y := 3
     x + y + 4")))
