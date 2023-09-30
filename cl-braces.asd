(in-package :asdf-user)

(defsystem "cl-braces"
  :description "A compiler playground"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.git")
  :pathname "src"
  :serial t
  :components ((:file "compiler"))
  :in-order-to ((test-op (test-op :cl-braces/test))))

;; (defsystm "cl-braces/executables"
;;   :description "Executable for cl-braces"
;;   :author "David Krentzlin <david.krentzlin@gmail.com>")


(defsystem "cl-braces/test"
  :description "Tests for cl-braces"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :pathname "tests"
  :serial t
  :components ((:file "test"))
  :perform (test-op (op c) (symbol-call :rove :run c)))
