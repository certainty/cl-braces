(in-package :cl-braces.tests.runner)

(defmacro with-env-specific-setup (&body body)
  (let ((db (gensym)))
    `(if (uiop:getenvp "CI_ENV")
         (let ((*debugger-hook*))
           (let ((,db (with-summary () ,@body)))
             (unless (and (null (lisp-unit2::failed-assertions ,db))
                          (null (errors ,db)))
               (uiop:quit 1))))
         (with-summary () ,@body))))

(defun system-path ()
  (asdf:system-source-directory "cl-braces/tests"))

(defun run-suites ()
  (run-vm-suites)
  (run-compiler-suites))

(defun run-vm-suites ()
  (let ((snapshots:*snapshot-dir* (merge-pathnames "tests/snapshots/vm/" (system-path))))
    (with-env-specific-setup
        (run-tests
         :name "vm"
         :package '(:tests.vm.bytecode :tests.vm.machine :tests.system)))))

(defun run-compiler-suites ()
  (let ((snapshots:*snapshot-dir* (merge-pathnames "tests/snapshots/compiler/" (system-path))))
    (with-env-specific-setup
        (run-tests
         :name "compiler"
         :package '(:tests.frontend.scanner :tests.frontend.parser :tests.frontend.ast :tests.backend.codegen :tests.middleend.symbols :tests.compiler.pipeline :tests.compiler.symbols)))))
