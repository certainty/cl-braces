(in-package :cl-braces.compiler.tests)

(defparameter *exit-on-failure* nil)

(defsuite compiler-suite ())
(defsuite frontend-suite (compiler-suite))

(defun run-all ()
  (let ((result (run-suite 'compiler-suite :report-progress nil)))
    (when (or (plusp (clunit::errors result))
              (plusp (clunit::failed result)))
      (when *exit-on-failure*
        (uiop:quit 1)))))
