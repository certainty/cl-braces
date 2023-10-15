(in-package :cl-braces/compiler/frontend)

(defclass scanner ()
  ((input :initarg :input :type source-input :documentation "The input to scan")
   (errors :initform (make-array 0 :fill-pointer 0 :element-type 'scan-error :adjustable t) :type (vector scan-error *) :reader scanner-errors :documentation "A list of errors encountered during scanning")
   (line :initform 1 :type integer :documentation "The current line number")
   (column :initform 0 :type integer :documentation "The current column number")
   (last-read-char :initform nil)
   (last-read-column :initform nil)
   (offset :initform 0 :type integer :documentation "The zero-based offset in the input to the start of the token.")))

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
      ((digit-char-p next) (scan-number scanner))
      (t (case (advance! scanner)
           (#\( (make-token :type +token-lparen+ :text "(" :location (location scanner)))
           (#\) (make-token :type +token-rparen+ :text ")" :location (location scanner)))
           (#\{ (make-token :type +token-lbrace+ :text "{" :location (location scanner)))
           (#\} (make-token :type +token-rbrace+ :text "}" :location (location scanner)))
           (#\[ (make-token :type +token-lbracket+ :text "[" :location (location scanner)))
           (#\] (make-token :type +token-rbracket+ :text "]" :location (location scanner)))
           (#\-
            (advance! scanner)
            (if (digit-char-p (peek scanner))
                (progn (retreat! scanner) (scan-number))
                (make-token :type +token-op-minus+ :text "-" :location (location scanner))))
           (#\+
            (advance! scanner)
            (if (digit-char-p (peek scanner))
                (progn (retreat! scanner) (scan-number))
                (make-token :type +token-op-plus+ :text "-" :location (location scanner))))

           (otherwise (illegal-token scanner "unexpected token")))))))

(defun eof-p (scanner)
  "Returns true if the scanner has reached the end of the input"
  (null (peek scanner)))

(defconstant +whitespace+ (list #\Space #\Tab #\Return #\Newline))

(defun skip-whitespaces (scanner)
  "Skip whitespaces and comments"
  (loop for next = (peek scanner) until (eof-p scanner) do
    (cond
      ((member next +whitespace+) (advance! scanner))
      ((char= #\/ next)
       (advance! scanner)
       (unless (char= #\/ (peek scanner))
         (retreat! scanner)
         (return))
       (loop for n = (advance! scanner) until (char= #\Newline n)))
      (t (return)))))

(-> advance! (scanner) (or null character))
(defun advance! (scanner)
  "Advance the scanner to the next character, adjusting internal state to keep track of location in the input stream.
   Returns the character that was advanced to or nil if the end of the input has been reached."
  (with-slots (last-read-char last-read-column column offset line input) scanner
    (let ((current-char (read-char (source-input-stream input) nil)))
      (setf last-read-char current-char)
      (setf last-read-column column)
      (incf column)
      (incf offset)
      (when (eql current-char #\Newline)
        (incf line)
        (setf column 0))
      current-char)))

(defun retreat! (scanner)
  "Unread the the last advanced character and rewind internal location tracking to previous location"
  (with-slots (last-read-char last-read-column column offset line input) scanner
    (unless (null last-read-char)

      (unread-char last-read-char (source-input-stream input))
      (setf column last-read-column)
      (decf offset)
      (when (char= last-read-char #\Newline)
        (decf line))
      (setf last-read-char nil)
      (setf last-read-column nil))))

(-> peek (scanner) (or null character))
(defun peek (scanner)
  "Peeks at the next character in the input stream without advancing the scanner."
  (with-slots (input) scanner
    (let ((stream (source-input-stream input)))
      (peek-char nil stream nil nil))))

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

(-> scan-number (scanner) token)
(defun scan-number (scanner)
  "Attempt to scan a number literal"
  (let ((loc (location scanner))
        (sign (peek scanner))
        (radix 10))

    (when (and sign (or (char= sign #\+) (char= sign #\-) (digit-char-p sign radix)))
      (advance! scanner))

    (let ((digits (scan-while scanner (lambda (c) (and c (digit-char-p c radix))))))
      (push sign digits)
      (let* ((digit-str (coerce digits 'string))
             (value (parse-integer digit-str :radix radix)))
        (make-token :type +token-number+ :text digit-str :value value :location loc)))))

(defun advance-if (scanner predicate)
  (when (funcall predicate (peek scanner))
    (advance! scanner)))

(defun scan-while (scanner predicate)
  "Consume the input, returning the list of consumed characters, while <predicate> returns t"
  (loop for next = (advance-if scanner predicate)
        until (null next)
        collect next))
