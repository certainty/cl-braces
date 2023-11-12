(in-package :cl-user)

(defpackage :cl-braces.tests.runner
  (:use :cl :lisp-unit2)
  (:export
   :run-vm-suites
   :run-compiler-suites))

;;; Compiler
(defpackage :cl-braces.tests.compiler.frontend.scanner
  (:nicknames :tests.frontend.scanner :tests.scanner)
  (:use :cl :lisp-unit2))

(defpackage :cl-braces.tests.compiler.frontend.parser
  (:nicknames :tests.frontend.parser :tests.parser)
  (:use :cl :lisp-unit2))

(defpackage :cl-braces.tests.compiler.frontend.ast
  (:nicknames :tests.frontend.ast :tests.ast)
  (:use :cl :lisp-unit2))

(defpackage :cl-braces.tests.compiler.backend.codegen
  (:nicknames :tests.backend.codegen :backend.codegen)
  (:use :cl :lisp-unit2))

(defpackage :cl-braces.tests.compiler.pipeline
  (:nicknames :tests.compiler.pipeline)
  (:use :cl :lisp-unit2))

;;; VM
(defpackage :cl-braces.tests.vm.bytecode
  (:nicknames :tests.vm.bytecode)
  (:use :cl :lisp-unit2))

(defpackage :cl-braces.tests.vm.machine
  (:nicknames :tests.vm.machine)
  (:use :cl :lisp-unit2))
