(in-package :cl-braces/compiler/tests)

(define-test :cl-braces/compiler/frontend/scanner-suite
  :parent :cl-braces/compiler-suite)


(define-test eof-p
  :parent :cl-braces/compiler/frontend/scanner-suite
  (let* ((input (source-input-open ""))
         (scanner (create-scanner input)))
    (true (eof-p scanner))))
