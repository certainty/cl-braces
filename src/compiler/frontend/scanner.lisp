(in-package :cl-braces/compiler/frontend)

(export '(scan-state make-scan-state scan-errors))
(defstruct (scan-state (:conc-name scan-))
  (input (error "input required") :type source-input)
  (errors (make-array 0 :fill-pointer 0 :element-type 'scan-error :adjustable t) :type (vector scan-error *))
  (offset 0 :type integer)
  (line 1 :type integer)
  (column 0 :type integer)
  (last-read-char nil :type (or null character))
  (last-read-column nil :type (or null integer)))

(defmethod print-object ((s scan-state) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (format stream "line:~a column:~a offset:~a errors:~a" (scan-line s) (scan-column s) (scan-offset s) (scan-errors s))))

(export '(scan-error scan-error-message scan-error-location))
(defstruct (scan-error (:conc-name scan-error-))
  (message (error "no message") :type string :read-only t)
  (location (error "no source-location") :type source-location :read-only t))


(defun string->scanner (s)
  (make-scan-state :input (source-input-open s)))

(export '(next-token))
(-> next-token (scan-state) token)
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
                (progn (retreat! scanner) (scan-number scanner))
                (make-token :type +token-op-minus+ :text "-" :location (location scanner))))
           (#\+
            (advance! scanner)
            (if (digit-char-p (peek scanner))
                (progn (retreat! scanner) (scan-number scanner))
                (make-token :type +token-op-plus+ :text "-" :location (location scanner))))
           (otherwise (illegal-token scanner "unexpected token")))))))

(export '(eof-p))
(defun eof-p (scanner)
  "Returns true if the scanner has reached the end of the input"
  (null (peek scanner)))

(defconst +whitespace+ (list #\Space #\Tab #\Return #\Newline))

(defun skip-whitespaces (scanner)
  "Skip whitespaces and comments"
  (loop for next = (peek scanner) until (or (eof-p scanner)) do
    (cond
      ((member next +whitespace+) (advance! scanner))
      ((eql #\/ next)
       (advance! scanner)
       (unless (eql #\/ (peek scanner))
         (retreat! scanner)
         (return))
       (loop for n = (advance! scanner) until (eql #\Newline n)))
      (t (return)))))

(-> advance! (scan-state) (or null character))
(defun advance! (scanner)
  "Advance the scanner to the next character, adjusting internal state to keep track of location in the input stream.
   Returns the character that was advanced to or nil if the end of the input has been reached."
  (let ((current-char (read-char (source-input-stream (scan-input scanner)) nil))
        (current-column (scan-column scanner)))
    (setf (scan-last-read-char scanner) current-char)
    (setf (scan-last-read-column scanner) current-column)
    (incf (scan-column scanner))
    (incf (scan-offset scanner))
    (when (eql current-char #\Newline)
      (incf (scan-line scanner))
      (setf (scan-column scanner) 0))
    current-char))

(-> retreat! (scan-state))
(defun retreat! (scanner)
  "Unread the the last advanced character and rewind internal location tracking to previous location"
  (unless (null (scan-last-read-char scanner))

    (unread-char (scan-last-read-char scanner) (source-input-stream (scan-input scanner)))
    (setf (scan-column scanner) (scan-last-read-column scanner))
    (decf (scan-offset scanner))
    (when (char= (scan-last-read-char scanner) #\Newline)
      (decf (scan-line scanner)))
    (setf (scan-last-read-char scanner) nil)
    (setf (scan-last-read-column scanner) nil)))

(-> peek (scan-state) (or null character))
(defun peek (scanner)
  "Peeks at the next character in the input stream without advancing the scanner."
  (let ((stream (source-input-stream (scan-input scanner))))
    (peek-char nil stream nil nil)))

(-> location (scan-state) source-location)
(defun location (scanner)
  "Return the current location in the source input"
  (make-source-location :line (scan-line scanner) :column (scan-column scanner) :offset (scan-offset scanner)))

(-> illegal-token (scan-state string &optional source-location) token)
(defun illegal-token (scanner message &optional loc)
  (let* ((effective-location (or loc (location scanner)))
         (err (make-scan-error :message message :location effective-location)))
    (vector-push-extend err (scan-errors scanner))
    (make-token :type +token-illegal+ :location effective-location)))

(-> scan-identifier (scan-state) token)
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

(-> scan-number (scan-state) token)
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
