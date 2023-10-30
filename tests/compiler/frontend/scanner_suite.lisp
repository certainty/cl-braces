(in-package :cl-braces.compiler.tests)

(defsuite scanner-suite (frontend-suite))

(defmacro assert-scans-as (input token-type)
  (let ((token-var (gensym)))
    `(let ((,token-var (scanner:call-with-scanner ,input #'scanner:next-token)))
       (assert-equality #'eql ,token-type (scanner:token-type ,token-var)))))

(deftest scan-eof (scanner-suite)
  "Make sure the scanner recognizes the end of the input"
  (assert-scans-as "" :tok-eof))

(deftest scan-illegal (scanner-suite)
  "All unknown tokens scan as illegal"
  (assert-scans-as "#unknown#" :tok-illegal))
