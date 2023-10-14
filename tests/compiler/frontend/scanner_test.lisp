(in-package :cl-braces/compiler/tests)

(define-test :cl-braces/compiler/frontend/scanner-suite
  :parent :cl-braces/compiler-suite)

(defun string->scanner (s)
  (create-scanner (source-input-open s)))

(define-test eof-p
  :parent :cl-braces/compiler/frontend/scanner-suite
  (true (eof-p (string->scanner ""))))

(define-test scan-keywords
  :parent :cl-braces/compiler/frontend/scanner-suite
  (is eql +token-kw-func+ (token-keyword (next-token (string->scanner "   func")))))
