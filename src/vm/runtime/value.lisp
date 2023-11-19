(in-package :vm.value)

(s:defunion value
  none ; the nil value
  (int (n integer))) ; numbers

(defgeneric box (n))

(defmethod box ((n integer))
  (int n))

(defmethod unbox ((v value:int))
  (int-n v))
