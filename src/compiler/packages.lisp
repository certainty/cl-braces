(in-package :cl-user)

(defpackage :cl-braces.compiler
  (:nicknames :compiler)
  (:use :cl)
  (:export
   :compile-this))

(defpackage :cl-braces.compiler.symbols
  (:nicknames :symbols)
  (:use :cl)
  (:import-from :serapeum :->)
  (:export
   :id
   :name
   :location
   :scope
   :denotation
   :symbol-table
   :make-symbol-table
   :denotes-variable-p
   :denotes-function-p
   :denotes-type-p
   :add-symbol
   :scope-t

   :find-by-id
   :find-by-name
   :filter-by-denotation
   :denotes-any
   :closest-scope))

(defpackage :cl-braces.compiler.location
  (:nicknames :compiler.location :location)
  (:use :cl)
  (:export
   :source-location
   :offset
   :line
   :column
   :make-source-location

   :source-span
   :span-from
   :span-to
   :span-for))
