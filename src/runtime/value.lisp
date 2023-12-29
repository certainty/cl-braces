(in-package :cl-braces.runtime.value)

(defclass <value> () ())

(defclass <nil>  (<value>) ())

(defvar *nil* (make-instance '<nil>))

(defun nilp (v)
  (typep v '<nil>))

(defun make-nil () *nil*)

(defclass <bool>  (<value>)
  ((value
    :reader bool-value
    :initform (error "No value")
    :initarg :value
    :type (member t nil))))

(defvar *true*  (make-instance '<bool> :value t))
(defvar *false* (make-instance '<bool> :value nil))

(defun truep (v)
  (bool-value v))

(defun falsep (v)
  (not (bool-value v)))

(defun boolp (v)
  (typep v '<bool>))

(defun make-bool (v)
  (etypecase v
    (boolean (if v *true* *false*))
    (t (error "Cannot make bool from ~s" v))))

(defclass <int>  (<value>)
  ((value
    :reader int-value
    :initform (error "No value")
    :initarg :value
    :type integer)))

(defun make-int (v)
  (etypecase v
    (integer (make-instance '<int> :value v))
    (t (error "Cannot make int from ~s" v))))

(defun intp (v)
  (typep v '<int>))

(defclass <arity> ()
  ((kind
    :initarg :kind
    :initform (error "No value")
    :type (member :fixed :variable))
   (value
    :initarg :value
    :initform (error "No value")
    :type (integer 0 *))))

(-> arity-exactly ((integer 0 *)) <arity>)
(defun arity-exactly (amount)
  (make-instance '<arity> :kind :fixed :value amount))

(-> arity-at-least ((integer 0 *)) <arity>)
(defun arity-at-least (amount)
  (make-instance '<arity> :kind :variable :value amount))

(defclass <closure>  (<value>)
  ((up-values
    :reader closure-up-values
    :initform (error "No value")
    :initarg :up-values
    :type (vector <value>))
   (arity
    :reader closure-arity
    :initform (error "No value")
    :initarg :arity
    :type <arity>)
   (registers-used
    :reader closure-registers-used
    :initform (error "No value")
    :initarg :registers-used
    :type (integer 0 *))
   (function-label
    :reader closure-function-label
    :initform (error "No value")
    :initarg :function-label
    :type bytecode:label)))

(-> make-closure (bytecode:label <arity> (integer 0 *) list) <closure>)
(defun make-closure (function-label arity registers-used &optional (upvalues nil))
  "Create a closure with the given `FUNCTION-RECORD-ADDRESS' and `UPVALUES'"
  (let ((up-values (make-array (length upvalues) :element-type 'value)))
    (make-instance '<closure>
                   :up-values up-values
                   :arity arity
                   :registers-used registers-used
                   :function-label function-label)))

(defun closurep (v)
  (typep v '<closure>))

(defun box (n)
  (etypecase n
    (integer (make-instance '<int> :value n))
    (boolean (make-instance '<bool> :value n))
    (symbol (case n
              (none (make-nil ))
              (t (error "Cannot box ~s" n))))))

(defun unbox (v)
  (etypecase v
    (<int> (int-value v))
    (<bool> (bool-value v))
    (<nil> nil)
    (t (error "Cannot unbox ~s" v))))
