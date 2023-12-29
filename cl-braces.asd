(in-package :asdf-user)

(defsystem  "cl-braces"
  :description "A compiler playground for a minimal go-like programming linguage, called `gone'"
  :long-description #.(uiop:read-file-string (uiop:subpathname *load-pathname* "README.md"))
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :maintainer "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.git")
  :license "BSD"
  :version "1.0"
  :depends-on (:alexandria :serapeum :frugal-uuid)
  :serial t
  :pathname "src"
  :in-order-to ((test-op (test-op "cl-braces/tests")))
  :components
  ((:file "packages")

   (:module "support"
    :components
    ((:file "utils")
     (:file "snapshot_tests")))

   (:module "runtime"
    :components
    ((:file "value")))

   (:module "bytecode"
    :components
    ((:file "chunk")
     (:file "isa")
     (:file "isa-1.0")
     (:file "disassembler")))

   (:module "sourcecode"
    :components
    ((:file "input")
     (:file "location")
     (:file "span")))

   (:module "compiler"
    :components
    ((:file "symbols")
     (:module "frontend"
      :components
      ((:module "lexer"
        :components
                ((:file "token")
                 (:file "scanner")))

       (:module "ast"
        :components
                ((:file "ast")
                 (:file "printer")))

       (:module "parser"
        :components
                ((:file "buffer")
                 (:file "parser")))))

     (:module "middleend"
      :components
      ((:module "semantic"
        :components
                ((:file "symbol-resolver")))))

     (:module "backend"
      :components
      ((:file "constants-builder")
       (:file "codegen")))

     (:file "pipeline")
     (:file "examples")))

   (:module "vm"
    :components
    ((:file "debug")
     (:file "callstack")
     (:file "machine")))))

(defsystem "cl-braces/tests"
  :depends-on (:lisp-unit2 :cl-braces)
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
     (:module "middleend"
      :components
      ((:file "symbol_resolver_suite")))
     (:module "backend"
      :components
      ((:file "codegen_suite")))
     (:file "symbols_suite")
     (:file "pipeline_suite")))
   (:module "vm"
    :components
    ((:file "bytecode_suite")
     (:file "machine_suite")
     (:file "system_suite"))))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call :cl-braces.tests.runner :run-suites)))
