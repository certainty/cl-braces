(in-package :cl-braces.vm.machine)


(defclass call-frame ()
  ((registers
    :initarg :registers
    :initform (vector))
   (return-address
    :initarg :return-address
    :initform (error "return-address not specified"))))

(defun make-call-frame (number-of-registers return-address)
  (make-instance 'call-frame :registers (make-registers number-of-registers) :return-address return-address))

(defun make-registers (amount)
  (make-array amount :element-type '(or null runtime.value:<value> bytecode:register-t) :initial-element nil))

(defclass call-stack ()
  ((stack-pointer
    :initarg :stack-pointer
    :initform 0
    :type a:array-index)
   (frames
    :initarg :frames
    :initform (error "frames not specified")
    :type (vector call-frame *))))

(defun make-call-stack (size)
  (make-instance 'call-stack :frames (make-array size :element-type 'call-frame :initial-element nil)))

(defun call-stack-reset (call-stack)
  (with-slots (stack-pointer frames) call-stack
    (setf stack-pointer 0)
    (fill frames nil)))

(defun call-stack-push (call-stack call-frame)
  (with-slots (stack-pointer frames) call-stack
    (setf (aref frames stack-pointer) call-frame)
    (incf stack-pointer)
    (setf (call-stack-stack-pointer call-stack) stack-pointer)))

(defun call-stack-pop (call-stack)
  (with-slots (stack-pointer frames) call-stack
    (decf stack-pointer)
    (let ((call-frame (aref frames stack-pointer)))
      (setf (aref frames stack-pointer) nil)
      call-frame)))

(defun call-stack-top (call-stack)
  (with-slots (stack-pointer frames) call-stack
    (when (call-stack-empty-p stack-pointer)
      (error "Call stack is empty"))
    (aref frames (1- stack-pointer))))

(defun call-stack-empty-p (call-stack)
  (with-slots (stack-pointer) call-stack
    (zerop stack-pointer)))

(defun call-stack-size (call-stack)
  (with-slots (stack-pointer) call-stack
    stack-pointer))
