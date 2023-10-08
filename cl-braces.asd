(asdf:defsystem  "cl-braces"
  :description "A compiler playground"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.git")
  :serial t
  :depends-on (:cl-braces/compiler))

(asdf:defsystem "cl-braces/compiler"
  :description "Compiler for cl-braces"
  :description "A compiler playground"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.git")
  :in-order-to ((test-op (test-op "cl-braces/compiler/tests")))
  :serial t
  :pathname "src/compiler"
  :components ((:module
                "frontend"
                :components ((:file "package")
                             (:file "input")
                             (:file "location")
                             (:file "scanner")))))

;; (defsystm "cl-braces/compiler/executable"
;;   :description "Executable for cl-braces"
;;   :author "David Krentzlin <david.krentzlin@gmail.com>")

(asdf:defsystem  "cl-braces/compiler/tests"
  :description "Tests for cl-braces compiler"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :depends-on (:parachute)
  :serial t
  :pathname "tests"
  :components ((:file "unit"))
  :perform (test-op (o c)
                    ( uiop:symbol-call :parachute :test :cl-braces/compiler/tests)))
