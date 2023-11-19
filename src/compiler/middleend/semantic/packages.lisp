(in-package :cl-user)

(defpackage :cl-braces.compiler.middleend.semantic.symbol-resolver
  (:nicknames :semantic.symbol-resolver :symbol-resolver)
  (:use :cl :cl-braces.development)
  (:import-from :serapeum :->)
  (:import-from :alexandria :when-let)
  (:export
   #:resolve-symbols))
