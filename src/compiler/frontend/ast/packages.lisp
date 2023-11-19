(in-package :cl-user)

(defpackage :cl-braces.compiler.frontend.ast
  (:nicknames :frontend.ast :ast)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:node
   #:location
   #:expression
   #:bad-expression
   #:literal
   #:literal-value

   #:grouping-expression
   #:grouping-expression-expression

   #:unary-expression
   #:unary-expression-operator
   #:unary-expression-operand

   #:binary-expression
   #:binary-expression-lhs
   #:binary-expression-operator
   #:binary-expression-rhs

   #:expression-list
   #:expression-list-expressions

   #:declaration
   #:bad-declaration
   #:short-variable-declaration
   #:short-variable-declaration-expressions
   #:short-variable-declaration-identifiers

   #:statement
   #:bad-statement
   #:empty-statement
   #:if-statement
   #:expression-statement
   #:expression-statement-expression
   #:statement-list
   #:statement-list-statements

   #:variable
   #:variable-identifier

   #:identifier
   #:identifier-token
   #:identifier-name
   #:identifier-list
   #:identifier-list-identifiers

   #:block
   #:block-statements

   #:program
   #:program-declarations
   #:make-program

   #:span
   #:span-from
   #:span-to

   #:walk
   #:enter
   #:leave
   #:with-preorder-traversal
   #:with-postorder-traversal
   #:*traversal*
   #:print-ast)
  (:shadow :declaration :variable :block))
