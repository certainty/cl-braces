(in-package :cl-user)

(defpackage :cl-braces.compiler.backend.codegen
  (:nicknames :compiler.backend.codegen :codegen)
  (:use :cl :cl-braces.development)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)

  (:import-from :cl-braces.vm.runtime.value :value)
  (:import-from :cl-braces.vm.bytecode :addr :reg :register :address)
  (:export
   #:generate-chunk))
