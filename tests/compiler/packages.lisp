(in-package :cl-user)

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
