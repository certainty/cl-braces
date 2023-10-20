(in-package :cl-braces/compiler/tests)

(define-test :cl-braces/compiler/frontend/scanner-suite
  :parent :cl-braces/compiler-suite)

;; (define-test test-eof-p
;;   :parent :cl-braces/compiler/frontend/scanner-suite
;;   (true (eof-p
;;          (string->scanner ""))))

(define-test skip-whitespaces
  :parent :cl-braces/compiler/frontend/scanner-suite
  (let ((comments (next-token (string->scanner (format nil "// foo bar~%valid"))))
        (newlines (next-token (string->scanner (format nil "   ~% bar")))))
    (is string= (token-text comments) "valid")
    (is string= (token-text newlines) "bar")))

(define-test next-token-smoke-test
  :parent :cl-braces/compiler/frontend/scanner-suite
  (let* ((s (string->scanner (format nil "// this is a comment ~% func main(){ return 3 }")))
         (tokens (scan-all s)))
    (is = 8 (length tokens))
    (is equal (mapcar #'token-type tokens) (list :tok-kw-func :tok-identifier :tok-lparen :tok-rparen :tok-lbrace :tok-kw-return :tok-number :tok-rbrace))))
