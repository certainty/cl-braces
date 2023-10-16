(in-package :cl-braces/compiler/tests)

(define-test :cl-braces/compiler/frontend/scanner-suite
  :parent :cl-braces/compiler-suite)

(defun string->scanner (s)
  (create-scanner (source-input-open s)))

(define-test eof-p
  :parent :cl-braces/compiler/frontend/scanner-suite
  (true (eof-p (string->scanner ""))))

(define-test skip-whitespaces
  :parent :cl-braces/compiler/frontend/scanner-suite
  (let ((comments (next-token (string->scanner (format nil "// foo bar~%valid"))))
        (newlines (next-token (string->scanner (format nil "foo    ~% bar")))))
    (is string= (token-text comments) "valid")))
