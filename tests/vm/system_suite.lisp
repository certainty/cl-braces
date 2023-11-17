(in-package :cl-braces.tests.system)

(defun compile-and-run (code)
  (value:unbox (machine:execute (compiler:compile-this code))))

(define-test simple-variables ()
  (assert-equal
   10
   (compile-and-run
    "x := 3
     y := 3
     x + y + 4")))

(define-test conditional-if ()
  (assert-equal
   10
   (compile-and-run "
   if true  {
     10
   } else {
     20
   }")))

(define-test conditional-if-shortstatement ()
  (assert-equal
   20
   (compile-and-run "
   lim := 10
   if v := 20; v < lim {
      v
   }
   ")))
