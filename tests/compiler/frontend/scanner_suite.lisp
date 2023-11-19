(in-package :tests.frontend.scanner)

(defmacro assert-token (token token-class &key (with-value nil))
  "Assert that the next token matches the given matcher. A mather is either a symbol denoting the expected token class or a list
where the car is the expected token class and the cadr is a keyword argument :with-value denoting the expected token value."
  `(progn
     (assert-eql ,token-class (token:class ,token))
     ,@(when with-value
         `((assert-equal ,with-value (token:value ,token))))))

(defmacro assert-scans-as (input &rest args)
  (let ((token-var (gensym)))
    `(let ((,token-var (scanner:call-with-scanner #'scanner:next-token ,input)))
       (assert-token ,token-var ,@args))))

(defmacro assert-scan-all-as (input &body token-matchers)
  "Scan input and assert that the tokens match the given matchers"
  (let ((scanner-var (gensym)))
    `(scanner:call-with-scanner
      (lambda (,scanner-var)
        ,@(mapcar (lambda (matcher)
                    `(assert-token (scanner:next-token ,scanner-var) ,matcher))
                  token-matchers))
      ,input)))

(define-test scan-eof ()
  "Scan the end of file"
  (assert-scans-as "" token:@EOF))

(define-test scan-illegal ()
  "Scan unknown characters as illegal tokens"
  (assert-scans-as "#unknown#" token:@ILLEGAL))

(define-test scan-inject-semicolon ()
  "Scan intertoken space and inject semicolon if required"
  (assert-scan-all-as (format nil "x := 3~% y") token:@IDENTIFIER token:@COLON_EQUAL token:@INTEGER token:@SEMICOLON token:@IDENTIFIER)
  (assert-scan-all-as (format nil "x~% y") token:@IDENTIFIER token:@SEMICOLON token:@IDENTIFIER)
  (assert-scan-all-as (format nil "break~% y") token:@BREAK token:@SEMICOLON token:@IDENTIFIER)
  (assert-scan-all-as (format nil "continue~% y") token:@CONTINUE token:@SEMICOLON token:@IDENTIFIER)
  (assert-scan-all-as (format nil "fallthrough~% y") token:@FALLTHROUGH token:@SEMICOLON token:@IDENTIFIER))

(define-test scan-integer ()
  "Scan an integer literal"
  (assert-scans-as "1234"  token:@INTEGER :with-value 1234)
  (assert-scans-as "12" token:@INTEGER :with-value 12)
  (assert-scans-as "0" token:@INTEGER :with-value 0))

(define-test location-tracking ()
  "Scan multiple tokens and track the location correctly for each"
  (let* ((s (scanner:open-scanner (format nil "   3~%4   5")))
         (t1 (scanner:next-token s))
         (semi (scanner:next-token s)) ;injected by scanner
         (t2 (scanner:next-token s))
         (t3 (scanner:next-token s)))

    (assert-equal 1 (location:line (token:location t1)))
    (assert-equal 4 (location:column (token:location t1)))
    (assert-equal 3 (location:offset (token:location t1)))

    (assert-eql token:@SEMICOLON (token:class semi))

    (assert-equal 2 (location:line (token:location t2)))
    (assert-equal 1 (location:column (token:location t2)))
    (assert-equal 5 (location:offset (token:location t2)))

    (assert-equal 2 (location:line (token:location t3)))
    (assert-equal 5 (location:column (token:location t3)))
    (assert-equal 9 (location:offset (token:location t3)))))

(define-test scan-unary-ops ()
  "Scan unary operators"
  (assert-scans-as "--"token:@MINUS_MINUS)
  (assert-scans-as "++"token:@PLUS_PLUS)
  (assert-scan-all-as "+ -" token:@PLUS token:@MINUS)
  (assert-scan-all-as "+3" token:@PLUS token:@INTEGER))

(define-test scan-binary-ops ()
  "Scan binary operators"
  (assert-scans-as "+" token:@PLUS)
  (assert-scans-as "-" token:@MINUS)
  (assert-scans-as "*" token:@STAR)
  (assert-scans-as "/" token:@SLASH)
  (assert-scans-as "<" token:@LT)
  (assert-scans-as "<=" token:@LE)
  (assert-scans-as ">" token:@GT)
  (assert-scans-as ">=" token:@GE))

(define-test scan-colon-equal ()
  "Scan the colon equal operator"
  (assert-scans-as ":=" token:@COLON_EQUAL))

(define-test scan-identifier ()
  "Scan identifiers"
  (assert-scans-as "foobar" token:@IDENTIFIER :with-value "foobar")
  (assert-scans-as "someVariable" token:@IDENTIFIER :with-value "someVariable")
  (assert-scans-as "some_function" token:@IDENTIFIER :with-value "some_function"))

(define-test scan-punctuation ()
  "Scan various punctuation tokens"
  (assert-scans-as " ;" token:@SEMICOLON)
  (assert-scans-as " ( " token:@LPAREN)
  (assert-scans-as ") " token:@RPAREN)
  (assert-scans-as "[" token:@LBRACKET)
  (assert-scans-as "]" token:@RBRACKET)
  (assert-scans-as "{" token:@LBRACE)
  (assert-scans-as "}" token:@RBRACE))

(define-test scan-keywords ()
  "Scan various key words"
  (assert-scans-as "if" token:@IF)
  (assert-scans-as "iffy" token:@IDENTIFIER :with-value "iffy")
  (assert-scans-as "else" token:@ELSE)
  (assert-scans-as "welser" token:@IDENTIFIER :with-value "welser")
  (assert-scans-as "break" token:@BREAK)
  (assert-scans-as "breaker" token:@IDENTIFIER :with-value "breaker")
  (assert-scans-as "continue" token:@CONTINUE)
  (assert-scans-as "discontinued" token:@IDENTIFIER :with-value "discontinued")

  (assert-scans-as "fallthrough" token:@FALLTHROUGH)
  (assert-scans-as "afallthroughb" token:@IDENTIFIER :with-value "afallthroughb"))
