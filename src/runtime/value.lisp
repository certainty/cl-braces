(in-package :cl-braces.runtime.value)

(s:defunion value
  nilv ; the nil value
  (boolv (b boolean)) ; booleans
  (intv (n integer))
  (closurev (c closure))) ; numbers

(s:defconstructor closure
  (up-values (vector value))
  (function-record-address bytecode:address-t))

(defun make-closure (function-record-address &optional (upvalues nil))
  "Create a closure with the given `FUNCTION-RECORD-ADDRESS' and `UPVALUES'"
  (let ((up-values (make-array (length upvalues) :element-type 'value)))
    (closurev (closure function-record-address up-values))))

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
