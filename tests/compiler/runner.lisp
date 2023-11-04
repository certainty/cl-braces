(in-package :cl-user)

(defpackage :cl-braces.tests.runner
  (:use :cl :lisp-unit2)
  (:export
   :run-all
   :run-all-ci
   :run-all-no-ci))

(in-package :cl-braces.tests.runner)

(defun run-suites ()
  (run-tests
   :name "compiler"
   :package '(:tests.frontend.scanner :tests.frontend.parser)))

(defun run-asdf ()
  (if (uiop:getenvp "CI_ENV")
      (run-all-ci)
      (run-all-no-ci)))

(defun run-all-ci ()
  (let (*debugger-hook*)
    (let ((db (with-summary ()
                (run-suites))))
      (unless (and
               (null (lisp-unit2::failed-assertions db))
               (null (errors db)))
        (uiop:quit 1)))))

(defun run-all-no-ci ()
  (with-summary ()
    (run-suites)))
