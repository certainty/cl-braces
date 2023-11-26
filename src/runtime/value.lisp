(in-package :cl-braces.runtime.value)

(s:defunion value
  nilv ; the nil value
  (boolv (b boolean)) ; booleans
  (intv (n integer))) ; numbers

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
    (t ninil)))

(defun truep (v)
  (typecase v
    (boolv (boolv-b v))
    (t nil)))
