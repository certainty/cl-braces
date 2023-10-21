(asdf:defsystem  "cl-braces"
  :description "A compiler playground"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.git")
  :serial t
  :depends-on (:cl-braces/compiler))

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
               )
  :in-order-to ((test-op (test-op "cl-braces/compiler/tests")))
  :serial t
  :pathname "src/compiler"
  :components ((:module
                "frontend"
                :components
                ((:file "package")

                 (:file "hr/scanner/package")
                 (:file "hr/scanner/scanner")
                 (:file "hr/scanner/input")
                 (:file "hr/scanner/location")
                 (:file "hr/scanner/token")


                 (:file "hr/parser/package")

                 ))


               (:file "package")
               (:file "compiler")))

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
                    (uiop:symbol-call :parachute :test :cl-braces/compiler/tests)))
