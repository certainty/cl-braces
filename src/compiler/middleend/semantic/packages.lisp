(in-package :cl-user)

(defpackage :cl-braces.compiler.middleend.semantic.symbol-resolver
  (:nicknames :semantic.symbol-resolver :symbol-resolver)
  (:use :cl :cl-braces.development)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:resolve-symbols))
