(in-package :cl-braces.compiler)

(defun compile-input (input)
  "Convenience function to compile a module from input string"
  (let* ((ast (introspection:mark-phase :parse (parser:parse input)))
         (typed-ast (introspection:mark-phase :typecheck (types:typecheck ast))))
    typed-ast))
