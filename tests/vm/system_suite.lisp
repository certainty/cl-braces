(in-package :cl-braces.tests.system)

(defun compile-and-run (code)
  (machine:run code))

(define-test simple-variables ()
  (assert-no-signal
   'error
   (compile-and-run
    "x := 3
     y := 4
     return x + y")))

(define-test statement-list ()
  (assert-no-signal
   'error
   (compile-and-run
    "x := 3; y := 4; return x + y")))

(define-test conditional-if ()
  (assert-no-signal
   'error
   (compile-and-run "
   if true  {
     10
   } else {
     20
   }

  return 30
  ")))

(define-test conditional-if-shortstatement ()
  (assert-no-signal
   'error
   (compile-and-run "
   lim := 10
   if v := 10; v == 10  {
      v
   }
   return lim
   ")))

(define-test assignment-and-declaration ()
  (assert-no-signal
   'error
   (compile-and-run "
   var x = 10
   var y = 20
   var (
     z int
     b int = 30
   )
   a := x + y + z
   return a + x + b
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
