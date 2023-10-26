(in-package :cl-user)

(defpackage :cl-braces.compiler.introspection
  (:nicknames :compiler.introspection :introspection)
  (:use :cl)
  (:export
   :tpe-compilation-phase
   :introspection-event
   :enter-phase
   :leave-phase
   :mark-phase))

(defpackage :cl-braces.compiler.frontend.scanner
  (:nicknames :compiler.scanner :frontend.scanner :scanner)
  (:use :cl :serapeum)
  (:export
   :source-origin
   :file-origin
   :file-origin-path
   :string-origin
   :string-origin-label
   :source-uri
   :source-input
   :source-input-stream
   :source-input-origin
   :source-input-open
   :source-input-close
   :call-with-source-input
   :with-source-input


   :source-location
   :make-source-location
   :location-line
   :location-column
   :location-offset

   :token
   :tpe-token
   :token-literal-p
   :token-keyword-p
   :token-eof-p
   :token-illegal-p
   :token-type
   :token-value
   :token-text
   :token-location

   :scan-state
   :make-scan-state
   :call-with-scanner
   :with-scanner
   :scan-errors
   :scan-error
   :scan-error-message
   :scan-error-location
   :scan-origin
   :eof-p

   :next-token
   :string->scanner
   :scan-all))

(defpackage :cl-braces.compiler.frontend.parser
  (:nicknames :compiler.parser :frontent.parser :parser)
  (:use :cl :serapeum)
  (:import-from :alexandria :positive-fixnum)
  (:export
   :*parser-fail-fast*
   :call-with-parser
   :with-parser
   :parse
   :ast-source))

(defpackage :cl-braces.compiler.frontend
  (:nicknames :compiler.frontend :frontend)
  (:use :cl :cl-braces.compiler.frontend.scanner :cl-braces.compiler.frontend.parser))

(defpackage :cl-braces.compiler
  (:nicknames :compiler)
  (:use :cl :serapeum)
  (:export :compile-input))
