(in-package :cl-user)

(defpackage :cl-braces.compiler
  (:nicknames :compiler)
  (:use :cl)
  (:export
   #:compile-this))

(defpackage :cl-braces.compiler.symbols
  (:nicknames :symbols)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)

  (:export
   #:id
   #:name
   #:location
   #:scope
   #:denotation
   #:symbol-table
   #:make-symbol-table
   #:denotes-variable-p
   #:denotes-function-p
   #:denotes-type-p
   #:place-holder-p
   #:add-symbol
   #:scope-t

   #:find-by-id
   #:find-by-name
   #:filter-by-denotation
   #:denotes-any
   #:closest-scope))

(defpackage :cl-braces.compiler.location
  (:nicknames :compiler.location :location)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:source-location
   #:offset
   #:line
   #:column
   #:make-source-location

   #:source-span
   #:span-from
   #:span-to
   #:span-for))
