(in-package :cl-braces/compiler/frontend/hr/scanner)

(defstruct (source-location (:conc-name location-))
  "A representation of a position some source input"
  (line   1 :type fixnum :read-only t)
  (column 1 :type fixnum :read-only t)
  (offset 0 :type fixnum :read-only t))
