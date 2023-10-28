(defpackage :cl-braces.utils
  (:use :cl))
(in-package :cl-braces.utils)

(defmacro todo (message)
  `(error "TODO: %s" ,message))

(defmacro unreachable (message)
  `(error "UNREACHABLE: %s" ,message))

(defmacro pry (&body form)
  (let ((result (gensym)))
    `(let ((,result (progn ,@form)))
       (format *debug-io* "~&👀  ~a => ~a" ',form ,result)
       ,result)))
