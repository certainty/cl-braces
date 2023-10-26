(in-package :cl-braces.compiler)

(defun compile-input (input)
  "Convenience function to compile a module from input string"
  (let ((ast (introspection:mark-phase :parse (parser:parse input))))
    ast))
