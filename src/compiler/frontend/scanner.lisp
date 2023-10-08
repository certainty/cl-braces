(in-package :cl-braces/compiler/frontend)

(defstruct scanner input start cursor line column errors)

(defun create-scanner (input)
  (declare (type source-input input))
  (make-scanner :input input
                :start 0
                :cursor 0
                :line 1
                :column 0
                :errors nil))

(defconstant +token-failure+ :failure)
(defconstant +token-eof+ :eof)

;; TODO: find out how I can make the slots required without providing a default
(defstruct token
  (type +token-eof+ :type symbol)
  (text "" :type string)
  (location nil :type (or null source-location)))

(declaim (ftype (function (scanner) token) next-token))
(defun next-token (scanner)
  "Scans the next token from input and return it.
This operation always succeeds unless a condition is raised.
If the input isn't recognized we simply return the special failure token and add the error to the internal scanner state.
"
  (declare (ignore scanner))
  (make-token :type +token-failure+))
