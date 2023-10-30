(in-package :cl-braces.compiler.tests)

(defsuite compiler-suite ())

(deftest trivially-true (compiler-suite)
  (assert-true (= 1 1)))

(defun run-all-with-exit ()
  (let ((result (run-suite 'compiler-suite :report-progress nil)))
    (when (or (plusp (clunit::errors result))
              (plusp (clunit::failed result)))
      (uiop:quit 1))))
