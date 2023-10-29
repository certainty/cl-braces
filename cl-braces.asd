(in-package :asdf-user)

(defsystem  "cl-braces"
  :description "A compiler playground"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.git")
  :depends-on (:cl-braces/compiler :cl-braces/vm)
  :serial t
  :components ((:file "scratch")))

(defsystem "cl-braces/compiler"
  :description "Compiler for cl-braces"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.git")
  :depends-on (
               :alexandria
               :serapeum
               :trivia
               :closer-mop
               :log4cl
               :cl-braces/shared
               )
  :in-order-to ((test-op (test-op "cl-braces/compiler/tests")))
  :serial t
  :pathname "src/compiler"
  :components ((:file "packages")
               (:file "introspection")

               (:module "frontend"
                :components
                ((:file "hr/scanner/location")
                 (:file "hr/scanner/input")
                 (:file "hr/scanner/token")
                 (:file "hr/scanner/scanner")
                 (:file "hr/ast/ast")
                 (:file "hr/parser/parser")))
               (:file "compiler")))

(defsystem "cl-braces/vm"
  :depends-on (
               :alexandria
               :serapeum
               :closer-mop
               :log4cl
               :cl-braces/shared)
  :serial t
  :pathname "src/vm"
  :components ((:file "packages")
               (:file "machine")))

(defsystem "cl-braces/shared"
  :depends-on (#:alexandria
               #:serapeum)
  :serial t
  :pathname   "src/shared"
  :components ((:module "utils"
                :components
                ((:file "packages")
                 (:file "utils")))

               (:module "runtime"
                :components
                ((:file "packages")
                 (:file "value")))

               (:module "isa"
                :components
                ((:file "packages")
                 (:file "instructions")
                 (:file "chunk")))))

;; (defsystm "cl-braces/compiler/executable"
;;   :description "Executable for cl-braces"
;;   :author "David Krentzlin <david.krentzlin@gmail.com>")

(defsystem  "cl-braces/compiler/tests"
  :description "Tests for cl-braces compiler"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :depends-on (:parachute :cl-braces/compiler)
  :serial t
  :pathname "tests/compiler"
  :components ((:file "suite")
               (:module "frontend"
                :components ((:file "hr/scanner/scanner_test"))))
  :perform (test-op (o c)
                    (uiop:symbol-call :parachute :test :cl-braces.compiler.tests)))
