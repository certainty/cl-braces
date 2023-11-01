(in-package :cl-braces.compiler.tests)

(defsuite scanner-suite (frontend-suite))

(defmacro assert-scans-as (input token-class &key (with-value nil))
  (let ((token-var (gensym)))
    `(let ((,token-var (scanner:call-with-scanner ,input #'scanner:next-token)))
       (assert-equalp ,token-class (token:class ,token-var))
       ,@(when with-value
           `((assert-equalp ,with-value (token:value ,token-var)))))))

(deftest scan-eof (scanner-suite)
  "Scan the end of file"
  (assert-scans-as "" token:@EOF))

(deftest scan-illegal (scanner-suite)
  "Scan unknown characters as illegal tokens"
  (assert-scans-as "#unknown#" token:@ILLEGAL))

(deftest scan-integer (scanner-suite)
  "Scan an integer literal"
  (assert-scans-as "1234"  token:@INTEGER :with-value 1234)
  (assert-scans-as "+1234" token:@INTEGER :with-value 1234)
  (assert-scans-as "-4231" token:@INTEGER :with-value -4231))
