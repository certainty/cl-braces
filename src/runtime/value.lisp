(in-package :cl-braces.runtime.value)

(s:defunion value
  nilv ; the nil value
  (boolv (b boolean)) ; booleans
  (intv (n integer))
  (closurev (c closure))) ; numbers

(s:defunion arity
  (arity-exactly (n fixnum))
  (arity-at-least (n fixnum)))

(s:defconstructor closure
  (arity arity)
  (body bytecode:chunk)
  (up-values (vector value)))

(defun make-closure (body arity &optional (upvalues nil))
  (let ((up-values (make-array (length upvalues) :element-type 'value)))
    (closurev (closure arity body up-values))))

(defun box (n)
  (etypecase n
    (integer (intv n))
    (boolean (boolv n))
    (symbol (case n
              (none nilv)
              (t (error "Cannot box ~s" n))))))

(defun unbox (v)
  (etypecase v
    (intv (intv-n v))
    (boolv (boolv-b v))
    (nilv 'none)))

(defun nonep (v)
  (typecase v
    (nilv t)
    (t nil)))

(defun falsep (v)
  (typecase v
    (boolv (not (boolv-b v)))
    (t nil)))

(defun truep (v)
  (typecase v
    (boolv (boolv-b v))
    (t nil)))
