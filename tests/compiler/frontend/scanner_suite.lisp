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

(deftest location-tracking (scanner-suite)
  "Scan multiple tokens and track the location correctly for each"
  (let* ((s (scanner:open-scanner (format nil "   3~%4   5")))
         (t1 (scanner:next-token s))
         (t2 (scanner:next-token s))
         (t3 (scanner:next-token s)))

    (assert-equalp 1 (token:location-line (token:location t1)))
    (assert-equalp 4 (token:location-column (token:location t1)))
    (assert-equalp 3 (token:location-offset (token:location t1)))

    (assert-equalp 2 (token:location-line (token:location t2)))
    (assert-equalp 1 (token:location-column (token:location t2)))
    (assert-equalp 5 (token:location-offset (token:location t2)))

    (assert-equalp 2 (token:location-line (token:location t3)))
    (assert-equalp 5 (token:location-column (token:location t3)))
    (assert-equalp 9 (token:location-offset (token:location t3)))))
