(in-package :cl-braces.compiler.tests)

(defsuite compiler-suite ())
(defsuite frontend-suite (compiler-suite))

(defun run-all ()
  (let ((result (run-suite 'compiler-suite :report-progress nil)))
    (when (or (plusp (clunit::errors result))
              (plusp (clunit::failed result)))
      (when (uiop:getenvp "CI_ENV")
        (uiop:quit 1)))))
