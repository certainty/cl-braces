(in-package :cl-braces/compiler/frontend)

(defclass scanner ()
  ((input :initarg :input :type source-input :reader scanner-input :documentation "The input to scan")
   (errors :initform (make-array 0 :fill-pointer 0 :element-type 'scan-error :adjustable t) :type (vector scan-error *) :accessor scanner-errors :documentation "A list of errors encountered during scanning")
   (line :initform 1 :type integer  :accessor scanner-line :documentation "The current line number")
   (column :initform 0 :type integer :accessor scanner-column :documentation "The current column number")
   (offset :initform 0 :type integer :accessor scanner-offset :documentation "The zero-based offset in the input to the start of the token.")))

(defclass scan-error ()
  ((message :initarg :message :type string)
   (location :initarg :location :type source-location)))

(defmethod print-object ((s scanner) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (with-slots (line column offset errors) s
      (format stream "line:~a column:~a offset:~a errors: ~a" line column offset errors))))

(defgeneric create-scanner (input)
  (:documentation "Creates a new scanner for the given input"))

(defmethod create-scanner ((input source-input))
  "Creates a new scanner for the given input"
  (make-instance 'scanner :input input))

(defun string->scanner (s)
  (create-scanner (source-input-open s)))

(-> next-token (scanner) token)
(defun next-token (scanner)
  "Scans the next token from input and return it.
This operation always succeeds unless a condition is raised.
If the input isn't recognized we simply return the special failure token and add the error to the internal scanner state.
"
  (skip-whitespaces scanner)

  (when (eof-p scanner)
    (return-from next-token (make-token :type +token-eof+)))

  (let ((next (peek scanner)))
    (cond
      ((or (alpha-char-p next) (char= #\_ next)) (scan-identifier scanner))
      (t (case (advance! scanner)
           (#\( (make-token :type +token-lparen+ :text "(" :location (location scanner)))
           (#\) (make-token :type +token-rparen+ :text ")" :location (location scanner)))
           (#\{ (make-token :type +token-lbrace+ :text "{" :location (location scanner)))
           (#\} (make-token :type +token-rbrace+ :text "}" :location (location scanner)))
           (#\[ (make-token :type +token-lbracket+ :text "[" :location (location scanner)))
           (#\] (make-token :type +token-rbracket+ :text "]" :location (location scanner)))
           (otherwise (illegal-token scanner "unexpected token")))))))

(defun eof-p (scanner)
  "Returns true if the scanner has reached the end of the input"
  (null (peek scanner)))

(defconstant +whitespace+ (list #\Space #\Tab #\Return #\Newline))

(defun skip-whitespaces (scanner)
  "Skip whitespaces and comments"
  (loop
    (cond
      ((eof-p scanner) (return))
      ((member (peek scanner) +whitespace+) (advance! scanner))
      (t (return)))))

(-> advance! (scanner) (or null character))
(defun advance! (scanner)
  "Advance the scanner to the next character, adjusting internal state to keep track of location in the input stream.
   Returns the character that was advanced to or nil if the end of the input has been reached."
  (let ((current-char (read-char (source-input-stream (scanner-input scanner)) nil)))
    (incf (scanner-column scanner))
    (incf (scanner-offset scanner))
    (when (eql current-char #\Newline)
      (incf (scanner-line scanner))
      (setf (scanner-column scanner) 0))
    current-char))

(-> peek (scanner) (or null character))
(defun peek (scanner)
  "Peeks at the next character in the input stream without advancing the scanner."
  (let ((stream (source-input-stream (scanner-input scanner))))
    (peek-char nil stream nil nil)))

(-> location (scanner) source-location)
(defun location (scanner)
  "Return the current location in the source input"
  (with-slots (column line offset) scanner
    (make-source-location :line line :column column :offset offset)))

(-> illegal-token (scanner string &optional source-location) token)
(defun illegal-token (scanner message &optional loc)
  (with-slots (errors) scanner
    (let* ((effective-location (or loc (location scanner)))
           (err (make-instance 'scan-error :message message :location effective-location)))
      (vector-push-extend err errors)
      (make-token :type +token-illegal+ :location effective-location))))

(-> scan-identifier (scanner) token)
(defun scan-identifier (scanner)
  "Attempt to scan an identifier or keyword"
  (let ((consumed (scan-while scanner #'identifier-char-p))
        (loc (location scanner)))

    (when (null consumed)
      (return-from scan-identifier (illegal-token scanner "expected identifier" loc)))

    (let* ((identifier (coerce consumed 'string))
           (kw (gethash identifier *string-to-keyword-type*)))
      (make-token :type (or kw +token-identifier+) :text identifier :location loc))))

(defun identifier-char-p (c)
  (and (characterp c)
       (or (sb-unicode:digit-value c) (sb-unicode:alphabetic-p c) (char= c #\_))))

(defun advance-if (scanner predicate)
  (when (funcall predicate (peek scanner))
    (advance! scanner)))

(defun scan-while (scanner predicate)
  "Consume the input, returning the list of consumed characters, while <predicate> returns t"
  (loop for next = (advance-if scanner predicate)
        until (null next)
        collect next))
