(in-package :cl-user)

(defpackage :cl-braces.tests.runner
  (:use :cl :lisp-unit2)
  (:export
   #:run-suites
   #:run-vm-suites
   #:run-compiler-suites))

(defpackage :cl-braces.tests.snapshots
  (:nicknames :cl-braces.snapshots :snapshots)
  (:use :cl :lisp-unit2)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:assert-snapshot-equals
   #:*snapshot-dir*))

;;; Compiler
(defpackage :cl-braces.tests.compiler.frontend.scanner
  (:nicknames :tests.frontend.scanner :tests.scanner)
  (:use :cl :lisp-unit2))

(defpackage :cl-braces.tests.compiler.frontend.parser
  (:nicknames :tests.frontend.parser :tests.parser)
  (:use :cl :lisp-unit2)
  (:import-from :alexandria :when-let))

(defpackage :cl-braces.tests.compiler.frontend.ast
  (:nicknames :tests.frontend.ast :tests.ast)
  (:use :cl :lisp-unit2))

(defpackage :cl-braces.tests.compiler.middlend.symbols
  (:nicknames :tests.middleend.symbols)
  (:use :cl :lisp-unit2))


(defpackage :cl-braces.tests.compiler.backend.codegen
  (:nicknames :tests.backend.codegen :backend.codegen)
  (:use :cl :lisp-unit2))

(defpackage :cl-braces.tests.compiler.pipeline
  (:nicknames :tests.compiler.pipeline)
  (:use :cl :lisp-unit2))

(defpackage :cl-braces.tests.compiler.symbols
  (:nicknames :tests.compiler.symbols)
  (:use :cl :lisp-unit2))

;;; VM
(defpackage :cl-braces.tests.vm.bytecode
  (:nicknames :tests.vm.bytecode)
  (:use :cl :lisp-unit2))

(defpackage :cl-braces.tests.vm.machine
  (:nicknames :tests.vm.machine)
  (:use :cl :lisp-unit2))

(defpackage :cl-braces.tests.system
  (:nicknames :tests.system)
  (:use :cl :lisp-unit2))
