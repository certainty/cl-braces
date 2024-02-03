(in-package :tests.vm.machine)

(define-test run-works ()
  "Tests that the machine can execute a simple program."
  (machine:run "return 3")
  (machine:run "return 3*4")
  (machine:run "return 3+3*3")
  (machine:run "return -(3+3*4)"))
