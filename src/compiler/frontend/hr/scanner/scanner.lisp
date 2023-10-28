(in-package :cl-braces.compiler.frontend.scanner)

(defparameter *fail-fast* nil "If true, the scanner will enter the debugger when an error is encountered")

(define-condition scan-error (error)
  ((message :initarg :message :reader scan-error-message)
   (location :initarg :location :reader scan-error-location))
  (:report (lambda (condition stream)
             (format stream "Illegal token at ~a" (scan-error-location condition)))))

(defclass state ()
  ((input :initform (error "input required") :initarg :input :type source-input)
   (input-stream :initform nil)
   (errors :initform (make-array 0 :element-type 'scan-error :adjustable t :fill-pointer 0) :type (vector scan-error *))
   (consumed :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer 0) :type (vector character *))
   (token-start :initform 0 :type integer)
   (offset :initform 0 :type integer)
   (line :initform 1 :type integer)
   (column :initform 0 :type integer)))

(defmethod initialize-instance :after ((s state) &key)
  (with-slots (input input-stream) s
    (setf input-stream (source-input-stream input))))

(defmethod print-object ((s state) stream)
  (with-slots (line column offset errors) s
    (print-unreadable-object (s stream :type t :identity t)
      (format stream "line:~a column:~a offset:~a errors:~a" line  column offset errors))))


(defun call-with-scanner (origin fn)
  (with-source-input (inp origin)
    (funcall fn (make-instance 'state :input inp))))

(defmacro with-scanner ((input-var origin) &body body)
  `(call-with-scanner ,origin (lambda (,input-var) ,@body)))

(defun string->scanner (s)
  (make-instance 'state :input (source-input-open s)))

(defun scan-all (scanner)
  "consumes all tokens and returns them in a list"
  (let ((tokens  (loop for tok = (next-token scanner) until (token-eof-p tok) collect tok)))
    (append tokens (list (make-token :type :tok-eof :location (location scanner))))))

(defun scan-origin (scanner)
  (with-slots (input) scanner
    (source-input-origin input)))

(-> next-token (state) token)
(defun next-token (scanner)
  (handler-bind ((scan-error (lambda (e) (if *fail-fast* (invoke-debugger e) (invoke-restart 'continue)))))
    (%next-token scanner)))

(defun %next-token (scanner)
  (with-slots (token-start offset) scanner
    (skip-whitespaces! scanner)
    (setf token-start offset)
    (or
     (scan-eof scanner)
     (scan-identifier scanner)
     (scan-number scanner)
     ;; (%scan-string scanner)
     (scan-operators scanner)
     (scan-punctuation scanner)
     (scan-illegal scanner))))

(defun accept (scanner token-type &key (convert-with #'identity))
  (with-slots (consumed offset token-start) scanner
    (let* ((token-text (coerce consumed 'string))
           (value (funcall convert-with token-text))
           (location (location scanner)))
      (setf consumed (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
      (setf token-start offset)
      (make-token :type token-type :text token-text :value value :location location))))

(defun scan-eof (scanner)
  (when (eof-p scanner)
    (accept scanner :tok-eof)))

(defun scan-illegal (scanner)
  "Consumes the next character and returns an illegal token"
  (advance! scanner)
  (accept scanner :tok-illegal))

(defun scan-identifier (scanner)
  (and-let* ((first-char (advance-when! scanner #'identifier-first-char-p)))
    (let* ((rest-chars (scan-while scanner #'identifier-char-p))
           (identifier (coerce (cons first-char rest-chars) 'string))
           (kw (gethash identifier *string-to-keyword*)))
      (accept scanner (or kw :tok-identifier)))))

(defun identifier-first-char-p (c)
  (and (characterp c)
       (or (alpha-char-p c) (char= c #\_))))

(defun identifier-char-p (c)
  (and (characterp c)
       (or (alphanumericp c) (char= c #\_))))

(defun scan-number (scanner)
  (multiple-value-bind (sign digit) (peek2 scanner)
    (cond
      ((and (or (eql #\+ sign) (eql #\- sign)) (digit-char-p digit))
       (advance! scanner)
       (scan-digits scanner)
       (accept scanner :tok-integer :convert-with #'parse-integer))
      ((scan-digits scanner)
       (accept scanner :tok-integer :convert-with #'parse-integer)))))

(defun scan-digits (scanner)
  (scan-while scanner (lambda (c) (and c (digit-char-p c)))))

(defun scan-operators (scanner)
  (multiple-value-bind (c1 c2) (peek2 scanner)
    (and-let* ((c1 c1)
               (c2 c2)
               (text (coerce (list c1 c2) 'string)))
      (macrolet ((=> (type) `(progn
                               (advance! scanner)
                               (advance! scanner)
                               (accept scanner ,type))))
        (cond
          ((string= text "++") (=> :tok-inc))
          ((string= text "+=") (=> :tok-plus-eql))
          ((string= text "--") (=> :tok-dec))
          ((string= text "-=") (=> :tok-minus-eql))
          ((string= text "*=") (=> :tok-mul-eql))
          ((string= text "!=") (=> :tok-bang-eql))
          ((string= text "==") (=> :tok-double-eql))
          ((string= text ":=") (=> :tok-colon-eql))
          ((string= text "&&") (=> :tok-double-ampersand))
          ((string= text "&=") (=> :tok-ampersand-eql))
          ((string= text "||") (=> :tok-double-pipe))
          ((string= text "|=") (=> :tok-pipe-eql))
          ((string= text "^=") (=> :tok-carret-eql))
          ((string= text "<<") (=> :tok-shift-left))
          ((string= text ">>") (=> :tok-shift-right))
          ((string= text "<=") (=> :tok-lt-eql))
          ((string= text ">=") (=> :tok-gt-eql))
          (t nil))))))

(defun scan-punctuation (scanner)
  (let ((c (peek scanner)))
    (macrolet ((=> (type) `(progn (advance! scanner) (accept scanner ,type))))
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
        (#\* (=> :tok-asterisk))
        (#\& (=> :tok-ampersand))
        (#\| (=> :tok-pipe))
        (#\^ (=> :tok-carret))
        (#\~ (=> :tok-tilde))
        (#\< (=> :tok-lt))
        (#\> (=> :tok-gt))
        (#\+ (=> :tok-plus))
        (#\- (=> :tok-minus))
        (t nil)))))

(-> advance! (state) (or null character))
(defun advance! (scanner &key (skip-p nil))
  "Advance the scanner to the next character, adjusting internal state to keep track of location in the input stream.
   Returns the character that was advanced to or nil if the end of the input has been reached."
  (with-slots (input-stream line offset column consumed) scanner
    (let ((current-char (read-char input-stream nil)))
      (when current-char
        (incf column)
        (incf offset)
        (if skip-p
            (setf token-start offset)
            (vector-push-extend current-char consumed))
        (when (eql current-char #\Newline)
          (incf line)
          (setf column 0))
        current-char))))

(-> peek (state) (or null character))
(defun peek (scanner)
  "Peeks at the next character in the input stream without advancing the scanner."
  (with-slots (input-stream) scanner
    (peek-char nil input-stream nil nil)))

(-> peek2 (state) (values (or null character) (or null character)))
(defun peek2 (scanner)
  "Peeks two characters ahead in the input stream without advancing the scanner."
  (with-slots (input-stream) scanner
    (let* ((c1 (read-char input-stream nil))
           (c2 (peek-char nil input-stream nil nil)))
      (when c1 (unread-char c1 input-stream))
      (values c1 c2))))

(-> eof-p (state) boolean)
(defun eof-p (scanner)
  "Returns true if the scanner has reached the end of the input"
  (null (peek scanner)))

(-> location (state) source-location)
(defun location (scanner)
  "Return the current location in the source input"
  (with-slots (line column token-start) scanner
    (make-source-location :line line :column column :offset token-start)))

(defconst +whitespace+ (list #\Space #\Tab #\Return #\Newline))
(defun skip-whitespaces! (scanner)
  "Skip whitespaces and comments"
  (loop for (c1 c2) = (multiple-value-list (peek2 scanner)) until (null c1) do
    (cond
      ((member c1 +whitespace+) (advance! scanner :skip-p t))
      ((and (eql #\/ c1) (eql #\/ c2))
       (advance! scanner :skip-p t)
       (advance! scanner :skip-p t)
       (loop for n = (advance! scanner :skip-p t) until (or (eql #\Newline n) (eof-p scanner))))
      (t (return)))))

(defun advance-when! (scanner predicate)
  (when (funcall predicate (peek scanner))
    (advance! scanner)))

(defun scan-while (scanner predicate)
  "Consume the input, returning the list of consumed characters, while <predicate> returns t"
  (loop for next = (advance-when! scanner predicate)
        until (null next)
        collect next))
