(in-package :cl-braces/compiler/frontend/hr/scanner)

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

(defstruct (scan-error (:conc-name scan-error-))
  (message (error "no message") :type string :read-only t)
  (location (error "no source-location") :type source-location :read-only t))

(defun string->scanner (s)
  (make-scan-state :input (source-input-open s)))

(defun scan-all (scanner)
  "consumes all tokens and returns them in a list"
  (let ((tokens  (loop for tok = (next-token scanner) until (token-eof-p tok) collect tok)))
    (append tokens (list (make-token :type :tok-eof :location (location scanner))))))

(-> next-token (scan-state) token)
(defun next-token (scanner)
  "Scans the next token from input and return it.
This operation always succeeds unless a condition is raised.
If the input isn't recognized we simply return the special failure token and add the error to the internal scanner state.
"
  (skip-whitespaces! scanner)
  (multiple-value-bind (c1 c2) (peek2 scanner)
    (let ((loc (location scanner)))
      (cond
        ((eof-p scanner) (make-token :type :tok-eof :location (location scanner)))
        ((identifier-first-char-p c1) (scan-identifier scanner))
        ((digit-char-p c1) (scan-number scanner))
        ((and (eql c1 #\-) (digit-char-p c2)) (scan-number scanner))
        ((and (eql c1 #\+) (digit-char-p c2)) (scan-number scanner))
        ((and (eql c1 #\!) (eql c2 #\=))
         (advance! scanner)
         (advance! scanner)
         (make-token :type :tok-op-bang-eql :text "!=" :location loc))
        ((and (eql c1 #\=) (eql c2 #\=))
         (advance! scanner)
         (advance! scanner)
         (make-token :type :tok-op-double-eql :text "!=" :location loc))
        (t (scan-single-char-token scanner))))))

(-> scan-single-char-token (scan-state) token)
(defun scan-single-char-token (scanner)
  (let* ((c (advance! scanner))
         (loc (location scanner)))
    (labels ((=> (type) (make-token :type type :text (string c) :location loc)))
      (case c
        (#\( (=> :tok-lparen))
        (#\) (=> :tok-rparen))
        (#\[ (=> :tok-lbracket))
        (#\] (=> :tok-rbracket))
        (#\{ (=> :tok-lbrace))
        (#\} (=> :tok-rbrace))
        (#\: (=> :tok-colon))
        (#\; (=> :tok-semicolon))
        (#\. (=> :tok-dot))
        (#\, (=> :tok-comma))
        (#\= (=> :tok-eql))
        (#\! (=> :tok-bang))
        (otherwise (illegal-token scanner "unexpected token"))))))

(-> eof-p (scan-state) boolean)
(defun eof-p (scanner)
  "Returns true if the scanner has reached the end of the input"
  (null (peek scanner)))

(defconst +whitespace+ (list #\Space #\Tab #\Return #\Newline))

(defun skip-whitespaces! (scanner)
  "Skip whitespaces and comments"
  (loop for (c1 c2) = (multiple-value-list (peek2 scanner)) until (null c1) do
    (cond
      ((member c1 +whitespace+) (advance! scanner))
      ((and (eql #\/ c1) (eql #\/ c2))
       (advance! scanner)
       (advance! scanner)
       (loop for n = (advance! scanner) until (or (eql #\Newline n) (eof-p scanner))))
      (t (return)))))

(-> advance! (scan-state) (or null character))
(defun advance! (scanner)
  "Advance the scanner to the next character, adjusting internal state to keep track of location in the input stream.
   Returns the character that was advanced to or nil if the end of the input has been reached."
  (let ((current-char (read-char (source-input-stream (scan-input scanner)) nil))
        (current-column (scan-column scanner)))
    (when current-char
      (setf (scan-last-read-char scanner) current-char)
      (setf (scan-last-read-column scanner) current-column)
      (incf (scan-column scanner))
      (incf (scan-offset scanner))
      (when (eql current-char #\Newline)
        (incf (scan-line scanner))
        (setf (scan-column scanner) 0))
      current-char)))

(-> peek (scan-state) (or null character))
(defun peek (scanner)
  "Peeks at the next character in the input stream without advancing the scanner."
  (let ((stream (source-input-stream (scan-input scanner))))
    (peek-char nil stream nil nil)))

(-> peek2 (scan-state) (values (or null character) (or null character)))
(defun peek2 (scanner)
  "Peeks two characters ahead in the input stream without advancing the scanner."
  (let* ((stream (source-input-stream (scan-input scanner)))
         (c1 (read-char stream nil))
         (c2 (peek-char nil stream nil nil)))
    (when c1
      (unread-char c1 stream))
    (values c1 c2)))

(-> location (scan-state) source-location)
(defun location (scanner)
  "Return the current location in the source input"
  (make-source-location :line (scan-line scanner) :column (scan-column scanner) :offset (scan-offset scanner)))

(-> illegal-token (scan-state string &optional source-location) token)
(defun illegal-token (scanner message &optional loc)
  (let* ((effective-location (or loc (location scanner)))
         (err (make-scan-error :message message :location effective-location)))
    (vector-push-extend err (scan-errors scanner))
    (make-token :type :tok-illegal :location effective-location)))

(-> scan-identifier (scan-state) token)
(defun scan-identifier (scanner)
  "Attempt to scan an identifier or keyword"
  (let* ((first-char (advance-when! scanner #'identifier-first-char-p))
         (rest-chars (scan-while scanner #'identifier-char-p)))
    (if (null first-char)
        (illegal-token scanner "Expected legal identifier character")
        (let* ((identifier (coerce (cons first-char rest-chars) 'string))
               (kw (gethash identifier *string-to-keyword*)))
          (make-token :type (or kw :tok-identifier) :text identifier :location (location scanner))))))

(defun identifier-first-char-p (c)
  (and (characterp c)
       (or (alpha-char-p c) (char= c #\_))))

(defun identifier-char-p (c)
  (and (characterp c)
       (or (alphanumericp c) (char= c #\_))))

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
        (make-token :type :tok-integer :text digit-str :value value :location loc)))))

(defun advance-when! (scanner predicate)
  (when (funcall predicate (peek scanner))
    (advance! scanner)))

(defun scan-while (scanner predicate)
  "Consume the input, returning the list of consumed characters, while <predicate> returns t"
  (loop for next = (advance-when! scanner predicate)
        until (null next)
        collect next))
