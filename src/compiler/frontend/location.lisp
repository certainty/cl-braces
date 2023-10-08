(in-package :cl-braces/compiler/frontend)

(defstruct source-location
  (line   1 :type fixnum :read-only t)
  (column 1 :type fixnum :read-only t)
  (offset 0 :type fixnum :read-only t))
