(in-package :cl-user)

(defpackage :cl-braces.compiler.frontend.ast
  (:nicknames :frontend.ast :ast)
  (:use :cl)
  (:import-from :serapeum :->)
  (:export
   :node
   :expression
   :bad-expression
   :literal
   :literal-value

   :grouping-expression
   :grouping-expression-expression

   :unary-expression
   :unary-expression-operator
   :unary-expression-operand

   :binary-expression
   :binary-expression-lhs
   :binary-expression-operator
   :binary-expression-rhs

   :declaration
   :bad-declaration
   :short-variable-declaration

   :statement
   :expression-statement
   :expression-statement-expression

   :program
   :program-declarations
   :make-program

   :span
   :span-from
   :span-to

   :walk
   :enter
   :leave
   :with-preorder-traversal
   :with-postorder-traversal
   :*traversal*
   :print-ast)
  (:shadow :declaration))
