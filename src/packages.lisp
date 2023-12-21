(in-package :cl-user)

(defpackage :cl-braces.support
  (:nicknames :support :support)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:todo!
   #:unreachable!
   #:pry
   #:returning
   #:domap
   #:define-enum
   #:debug-print
   #:list-of
   #:non-empty-list-of
   #:to-plist
   #:copy-instance))

(defpackage :cl-braces.support.tests.snapshots
  (:nicknames :cl-braces.snapshots :snapshots)
  (:use :cl :lisp-unit2)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:assert-snapshot-equals
   #:*snapshot-dir*))

;;;
;;; Runtime
;;;

(defpackage :cl-braces.runtime.value
  (:nicknames :runtime.value)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:value
   #:intV
   #:boolv
   #:nilv
   #:box
   #:unbox
   #:falsep
   #:truep
   #:nonep
   #:make-closure
   #:arity
   #:arity-exactly
   #:arity-at-least))

(defpackage :cl-braces.bytecode
  (:nicknames :bytecode)
  (:use :cl :cl-braces.support)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:chunk
   #:make-chunk
   #:chunk-code
   #:chunk-constants
   #:chunk-registers-used
   #:constant-table



   #:make-constants-builder
   #:constants-add
   #:make-chunk-builder
   #:instruction-opcode
   #:instruction-operands

   #:add-constant
   #:add-instructions

   #:go-package
   #:package-builder
   #:make-package-builder
   #:add-closure

   #:print-isa
   #:*isa-1.0*
   #:*current-isa*
   #:with-opcodes-from-current-isa
   #:operand-value
   #:address-value
   #:register-value
   #:disass
   #:disass-instruction

   #:address-t
   #:register-t
   #:opcode-t

   #:instr
   #:address
   #:addr
   #:register
   #:reg
   #:label

   #:const
   #:mov
   #:test
   #:jz
   #:jnz
   #:jmp
   #:noop
   #:halt
   #:add
   #:sub
   #:div
   #:mul
   #:neg))

;;;
;;; Source code
;;;

(defpackage :cl-braces.sourcecode
  (:nicknames :sourcecode)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:open-source-input
   #:close-source-input
   #:source-input
   #:string-input
   #:source-input-stream
   #:open-input
   #:input-designator
   #:call-with-input
   #:with-input))

(defpackage :cl-braces.sourcecode.location
  (:nicknames :sourcecode.location :location)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:source-location
   #:offset
   #:line
   #:line++
   #:column
   #:column++
   #:make-source-location))

(defpackage :cl-braces.sourcecode.span
  (:nicknames :sourcecode.span :span)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:source-span
   #:make-span
   #:from
   #:to
   #:for))

;;;
;;; Compiler
;;;

(defpackage :cl-braces.compiler.frontend.token
  (:nicknames :frontend.token :token)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:shadow :class)
  (:import-from :serapeum :->)
  (:export
   #:token
   #:class
   #:lexeme
   #:value
   #:location
   #:token-class
   #:class=
   #:literal-p
   #:identifier-p
   #:punctuation-p
   #:class-any-p
   #:synthetic-eof

   #:@EOF
   #:@ILLEGAL
   #:@LPAREN
   #:@RPAREN
   #:@LBRACKET
   #:@RBRACKET
   #:@LBRACE
   #:@RBRACE
   #:@INTEGER
   #:@PLUS
   #:@PLUS_PLUS
   #:@MINUS
   #:@MINUS_MINUS
   #:@STAR
   #:@SLASH
   #:@LT
   #:@LE
   #:@GT
   #:@GE
   #:@SEMICOLON
   #:@COMMA
   #:@COLON_EQUAL
   #:@PLUS_EQUAL
   #:@MUL_EQUAL
   #:@EQUAL
   #:@EQUAL_EQUAL
   #:@IDENTIFIER
   #:@TRUE
   #:@FALSE
   #:@NIL
   #:@FUNC
   #:@IF
   #:@ELSE
   #:@BREAK
   #:@CONTINUE
   #:@FALLTHROUGH
   #:@RETURN
   #:@VAR
   #:@ELLIPSIS))

(defpackage :cl-braces.compiler.frontend.scanner
  (:nicknames :frontend.scanner :scanner :frontend.lexer :lexer)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:scan-all
   #:make-scanner
   #:next-token))

(defpackage :cl-braces.compiler.frontend.ast
  (:nicknames :compiler.frontend.ast :ast)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:shadow :declaration :variable :block)
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
   #:variable-declaration
   #:variable-declaration-specifications
   #:variable-specification
   #:variable-specification-identifiers
   #:variable-specification-type
   #:variable-specification-initializer

   #:statement
   #:bad-statement
   #:empty-statement
   #:if-statement
   #:if-statement-init
   #:if-statement-condition
   #:if-statement-consequence
   #:if-statement-alternative
   #:expression-statement
   #:expression-statement-expression
   #:statement-list
   #:statement-list-statements

   #:assignment-statement
   #:assignment-statement-lhs
   #:assignment-statement-operator
   #:assignment-statement-rhs

   #:variable
   #:variable-identifier

   #:type-specifier
   #:type-specifier-name

   #:identifier
   #:identifier-token
   #:identifier-name
   #:identifier-list
   #:identifier-list-identifiers

   #:function-declaration
   #:function-declaration-name
   #:function-declaration-signature
   #:function-declaration-body

   #:function-signature
   #:function-signature-parameters
   #:function-signature-return-type
   #:function-signature-return-parameters

   #:parameter-declaration
   #:parameter-declaration-identifiers
   #:parameter-declaration-splat
   #:parameter-declaration-type

   #:parameter-splat
   #:parameter-splat-token

   #:parameter-list
   #:parameter-list-parameters

   #:comma

   #:block
   #:block-statements

   #:source-file
   #:source-file-declarations
   #:make-source-file

   #:walk
   #:enter
   #:leave
   #:print-ast))


(defpackage :cl-braces.compiler.frontend.parser
  (:nicknames :frontend.parser :parser)
  (:use :cl :cl-braces.support)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:parse
   #:parse-errors))


(defpackage :cl-braces.compiler.middleend.semantic.symbol-resolver
  (:nicknames :semantic.symbol-resolver :symbol-resolver)
  (:use :cl :cl-braces.support)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:resolve-symbols))

(defpackage :cl-braces.compiler.backend.codegen
  (:nicknames :compiler.backend.codegen :codegen)
  (:use :cl :cl-braces.support)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)

  (:import-from :cl-braces.runtime.value :value)
  (:import-from :cl-braces.bytecode :addr :reg :register :address)
  (:export
   #:generate-chunk
   #:generate-package))

(defpackage :cl-braces.compiler
  (:nicknames :compiler)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:compile-this
   #:compile-package))

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

(defpackage :cl-braces.compiler.examples
  (:nicknames :compiler.examples)
  (:use :cl :cl-braces.support))

(defpackage :cl-braces.vm.machine
  (:nicknames :vm.machine :machine)
  (:use :cl :cl-braces.support)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:import-from :cl-braces.bytecode)
  (:export
   #:run
   #:execute))
