(in-package :tests.vm.machine)

(define-test execute-works ()
  "Tests that the machine can execute a simple program."
  (assert-equal 3 (value:unbox (machine:execute (compiler:compile-this "3"))))
  (assert-equal 12 (value:unbox (machine:execute (compiler:compile-this "3*4"))))
  (assert-equal 12 (value:unbox (machine:execute (compiler:compile-this "3+3*3"))))
  (assert-equal -15 (value:unbox (machine:execute (compiler:compile-this "-(3+3*4)")))))
