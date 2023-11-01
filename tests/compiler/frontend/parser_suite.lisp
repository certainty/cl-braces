(in-package :cl-braces.compiler.tests)

(defsuite parser-suite (frontend-suite))

(deftest parse-integer-literal (parser-suite)
  "Parse an integer literal"
  (let ((node (parser:parse "3")))
    (assert-equalp (type-of node) 'ast:literal)
    (assert-equalp (ast:literal-value node) 3)))
