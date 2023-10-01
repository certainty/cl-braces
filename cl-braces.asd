(asdf:defsystem  "cl-braces"
  :description "A compiler playground"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.git")
  :serial t
  :components ((:module "src"
                :components ((:file "compiler"))))
  :in-order-to ((test-op (test-op "cl-braces/tests"))))

;; (defsystm "cl-braces/executables"
;;   :description "Executable for cl-braces"
;;   :author "David Krentzlin <david.krentzlin@gmail.com>")

(asdf:defsystem  "cl-braces/tests"
  :description "Tests for cl-braces"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :depends-on (:fiveam)
  :serial t
  :components ((:module "tests"
                :components ((:file "unit"))))

  :perform (test-op (o c)
                    (unless (symbol-call '#:fiveam '#:run!
                                         :cl-braces/tests)
                      (error "TESTS failed!"))))
