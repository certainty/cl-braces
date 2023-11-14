(in-package :asdf-user)

(defsystem  "cl-braces"
  :description "A compiler playground for a minimal go-like programming linguage, called `gone'"
  :long-description #.(uiop:read-file-string (uiop:subpathname *load-pathname* "README.md"))
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :maintainer "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.git")
  :license "BSD"
  :version "1.0"
  :serial t
  :depends-on (:cl-braces/compiler))

(defsystem "cl-braces/utils"
  :description "Utilities used in different parts of the project"
  :depends-on  (:alexandria :serapeum :lisp-unit2)
  :serial t
  :pathname "src/utils"
  :components ((:file "packages")
               (:file "development")
               (:file "tests")))

(defsystem "cl-braces/compiler"
  :description "Compiler for cl-braces the minimal go-like programming language"
  :depends-on (:alexandria :serapeum :cl-braces/vm :cl-braces/utils)
  :in-order-to ((test-op (test-compiler-op "cl-braces/tests")))
  :serial t
  :pathname "src/compiler"
  :components ((:file "packages")
               (:file "location")
               (:module "frontend"
                :components
                ((:module "scanner"
                  :components
                  ((:file "packages")
                   (:file "input")
                   (:file "token")
                   (:file "scanner")))
                 (:module "ast"
                  :components
                  ((:file "packages")
                   (:file "ast")
                   (:file "printer")))
                 (:module "parser"
                  :components
                  ((:file "packages")
                   (:file "parser")))))
               (:module "backend"
                :components
                ((:file "packages")
                 (:file "constants-builder")
                 (:file "chunk-builder")
                 (:file "codegen")))
               (:file "pipeline")))

(defsystem "cl-braces/vm"
  :description "Compiler for cl-braces the minimal go-like programming language"
  :depends-on (:alexandria :serapeum :cl-braces/utils)
  :in-order-to ((test-op (test-vm-op "cl-braces/tests")))
  :serial t
  :pathname "src/vm"
  :components ((:file "packages")
               (:module "runtime"
                :components
                ((:file "value")))
               (:module "bytecode"
                :components
                ((:file "chunk")
                 (:file "isa")
                 (:file "disassembler")
                 (:file "isa-1.0")))
               (:file "machine")))

(defclass test-vm-op (asdf:test-op) ())
(defclass test-compiler-op (asdf:test-op) ())

(defsystem "cl-braces/tests"
  :depends-on (:lisp-unit2 :cl-braces/compiler :cl-braces/vm :cl-braces/utils)
  :serial t
  :pathname "tests"
  :components
  ((:file "packages")
   (:file "runner")
   (:module "compiler"
    :components
    ((:module "frontend"
      :components
      ((:file "scanner_suite")
       (:file "parser_suite")))
     (:module "backend"
      :components
      ((:file "codegen_suite")))
     (:file "pipeline_suite")))
   (:module "vm"
    :components
    ((:file "bytecode_suite")
     (:file "machine_suite")
     (:file "system_suite"))))
  :perform (test-vm-op (o c)
                       (declare (ignore o c))
                       (uiop:symbol-call :cl-braces.tests.runner :run-vm-suites))

  :perform (test-compiler-op (o c)
                             (declare (ignore o c))
                             (uiop:symbol-call :cl-braces.tests.runner :run-compiler-suites)))
