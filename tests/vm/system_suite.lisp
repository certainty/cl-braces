(in-package :cl-braces.tests.system)

(defun compile-and-run (code)
  (runtime.value:unbox (machine:execute (compiler:compile-string code))))

(define-test simple-variables ()
  (assert-equal
   7
   (compile-and-run
    "x := 3
     y := 4
     x + y")))

(define-test statement-list ()
  (assert-equal
   7
   (compile-and-run
    "x := 3; y := 4; x + y")))

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
   30
   (compile-and-run "
   lim := 10
   if v := 10; v == 10  {
      v
   }
   ")))

(define-test assignment-and-declaration ()
  (assert-equal
   90
   (compile-and-run "
   var x = 10
   var y = 20
   var (
     z int
     b int = 30
   )
   a := x + y + z
   a + x + b
   ")))

;; (define-test functions-with-one-return-value ()
;;   (assert-equal
;;    10
;;    (compile-and-run "
;;    func f() int {
;;      x := 42
;;      if true {
;;         return x
;;      } else {
;;        return 33
;;      }
;;    }
;;    f()
;;    "))
;;   )
