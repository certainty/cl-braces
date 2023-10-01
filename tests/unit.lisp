(defpackage :cl-braces/tests
  (:use #:cl #:fiveam))

(in-package :cl-braces/tests)

(def-suite  :cl-braces :description "All CL-BRACES tests")
(def-suite* :cl-braces/tests :in :cl-braces)

(test test-1
  "Just a test"
  (is (= 1 1)))
