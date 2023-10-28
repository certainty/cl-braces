(asdf:defsystem  "cl-braces"
  :description "A compiler playground"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.git")
  :serial t
  :depends-on (:cl-braces/compiler :cl-braces/vm))

(asdf:defsystem "cl-braces/compiler"
  :description "Compiler for cl-braces"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.git")
  :depends-on (
               #:alexandria
               #:serapeum
               #:defstar

               #:trivia
               #:closer-mop

               #:log4cl

               #:cl-braces/isa
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

(asdf:defsystem "cl-braces/vm"
  :description "vm for cl-braces"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.git")
  :depends-on (
               #:alexandria
               #:serapeum
               #:defstar

               #:trivia
               #:closer-mop

               #:log4cl

               #:cl-braces/isa
               )
  :serial t
  :pathname "src/vm"
  :components ((:file "packages")
               (:file "machine")))

(asdf:defsystem "cl-braces/isa"
  :description "Shared code between VM and Compiler for cl-braces"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.git")
  :depends-on (#:alexandria
               #:serapeum)
  :serial t
  :pathname "src/isa"
  :components ((:file "packages")
               (:file "instructions")
               (:file "value")
               (:file "chunk")))


;; (defsystm "cl-braces/compiler/executable"
;;   :description "Executable for cl-braces"
;;   :author "David Krentzlin <david.krentzlin@gmail.com>")

(asdf:defsystem  "cl-braces/compiler/tests"
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
