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
    `(let ((,token-var (scanner:call-with-scanner ,input #'scanner:next-token)))
       (assert-token ,token-var ,@args))))

(defmacro assert-scan-all-as (input &body token-matchers)
  "Scan input and assert that the tokens match the given matchers"
  (let ((scanner-var (gensym)))
    `(scanner:call-with-scanner ,input
      (lambda (,scanner-var)
        ,@(mapcar (lambda (matcher)
                    `(assert-token (scanner:next-token ,scanner-var) ,matcher))
                  token-matchers)))))

(define-test scan-eof ()
  "Scan the end of file"
  (assert-scans-as "" token:@EOF))

(define-test scan-illegal ()
  "Scan unknown characters as illegal tokens"
  (assert-scans-as "#unknown#" token:@ILLEGAL))

(define-test scan-integer ()
  "Scan an integer literal"
  (assert-scans-as "1234"  token:@INTEGER :with-value 1235)
  (assert-scans-as "12" token:@INTEGER :with-value 12)
  (assert-scans-as "0" token:@INTEGER :with-value 0))

(define-test location-tracking ()
  "Scan multiple tokens and track the location correctly for each"
  (let* ((s (scanner:open-scanner (format nil "   3~%4   5")))
         (t1 (scanner:next-token s))
         (t2 (scanner:next-token s))
         (t3 (scanner:next-token s)))

    (assert-equal 1 (token:location-line (token:location t1)))
    (assert-equal 4 (token:location-column (token:location t1)))
    (assert-equal 3 (token:location-offset (token:location t1)))

    (assert-equal 2 (token:location-line (token:location t2)))
    (assert-equal 1 (token:location-column (token:location t2)))
    (assert-equal 5 (token:location-offset (token:location t2)))

    (assert-equal 2 (token:location-line (token:location t3)))
    (assert-equal 5 (token:location-column (token:location t3)))
    (assert-equal 9 (token:location-offset (token:location t3)))))

(define-test scan-unary-ops ()
  "Scan unary operators"
  (assert-scan-all-as "+ -" token:@PLUS token:@MINUS)
  (assert-scan-all-as "+3" token:@PLUS token:@INTEGER))
