(in-package :cl-braces.compiler.backend.bytecode-generator)

(defclass constant-table-builder () ())

(defclass chunk-builder () ())

(defclass code-generator () ())

(defgeneric build (builder))

(defgeneric emit (generator ast:node))

(defmethod emit ((generator code-generator) (expr ast:literal-expression))
  (todo! "Not implemented"))

(defmethod emit ((generator code-generator) (expr ast:identifier))
  (todo! "Not implemented"))

(defmethod emit ((generator code-generator) (expr ast:binary-expression))
  (todo! "Not implemented"))
