(in-package :cl-braces.support)

(defmacro todo! (message)
  (let ((full-message (gensym)))
    `(let ((,full-message (concatenate 'string "TODO: " ,message)))
       (error ,full-message))))

(defmacro unreachable! (message)
  (let ((full-message (gensym)))
    `(let ((,full-message (concatenate 'string "UNREACHABLE: " ,message)))
       (error ,full-message))))

(defmacro pry (&body form)
  (let ((result (gensym)))
    `(let ((,result  (progn ,@form)))
       (prog1 ,result
         (format *debug-io* "~&👀  ~a => ~a" ',@form ,result)))))

(defmacro returning ((var expr) &body body)
  `(let ((,var ,expr))
     (prog1 ,var ,@body)))

(defmacro domap ((var list) &body body)
  `(mapcar (lambda (,var) ,@body) ,list))

(defmacro define-enum (name &rest variants)
  (let ((iota 0))
    `(progn
       ,@(mapcar (lambda (variant)
                   (prog1 `(defconstant ,(intern (format nil "+~A-~A+" name variant) *package*) ,iota)
                     (incf iota)))
                 variants)
       (deftype ,(intern (format nil "~A" name) *package*) () '(integer 0 ,iota)))))

(defgeneric debug-print (obj)
  (:documentation "Prints a debug representation of OBJ to STREAM."))

(deftype list-of (type)
  `(or null (cons ,type ,type)))

(deftype none-empty-list-of (type)
  `(cons ,type ,type))
