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

(defsystem "cl-braces/compiler"
  :description "Compiler for cl-braces the minimal go-like programming language"
  :depends-on (:alexandria :serapeum)
  :in-order-to ((test-op (test-op "cl-braces/compiler/tests")))
  :serial t
  :pathname "src/compiler"
  :components ((:file "../development")
               (:file "packages")
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
                 (:file "codegen")))
               (:file "pipeline")))

(defsystem "cl-braces/compiler/tests"
  :depends-on (:lisp-unit2 :cl-braces/compiler)
  :serial t
  :pathname "tests/compiler"
  :components
  ((:file "packages")
   (:module "frontend"
    :components
    ((:file "scanner_suite")
     (:file "parser_suite")))
   (:file "runner"))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call :cl-braces.tests.runner :run-asdf)))
