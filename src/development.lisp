(in-package :cl-user)

(defpackage :cl-braces.development
  (:nicknames :dev)
  (:use :cl)
  (:export
   :todo!
   :unreachable!
   :pry
   :returning
   :domap
   ))

(in-package :cl-braces.development)

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
