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
               :cl-braces/vm
               )
  :in-order-to ((test-op (test-op "cl-braces/compiler/tests")))
  :serial t
  :pathname "src/compiler"
  :components ((:file "packages")
               (:file "introspection")
               (:module "frontend"
                :components
                ((:module "highlevel"
                  :components

                  ((:module "scanner"
                    :components
                            ((:file "location")
                             (:file "input")
                             (:file "token")
                             (:file "scanner")))

                   (:file "ast")
                   (:file "parser")

                   (:module "types"
                    :components
                            ((:file "checker")))))

                 (:module "intermediate"
                  :components
                  ((:file "packages")))))

               (:module "backend"
                :components
                ((:file "packages")
                 (:file "bytecode-generator")))

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

               (:module "runtime"
                :components
                ((:file "value")))

               (:module "bytecode"
                :components
                ((:file "instructions")
                 (:file "chunk")))

               (:file "disassembler")
               (:file "machine")))

(defsystem "cl-braces/shared"
  :depends-on (#:alexandria
               #:serapeum)
  :serial t
  :pathname   "src/shared"
  :components ((:module "utils"
                :components
                ((:file "packages")
                 (:file "utils")))))

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
