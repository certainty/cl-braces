(in-package :cl-braces.utils)

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
    `(let ((,result (progn ,@form)))
       (format *debug-io* "~&👀  ~a => ~a" ',form ,result)
       ,result)))
