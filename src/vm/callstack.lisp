(in-package :cl-braces.vm.machine)

(define-condition stack-overflow-error (error)
  ((call-stack
    :reader stack-overflow-error-call-stack
    :initarg :call-stack)))

(defclass call-frame ()
  ((registers
    :reader call-frame-registers
    :initarg :registers
    :initform (vector))
   (return-address
    :reader call-frame-return-address
    :initarg :return-address
    :initform (error "return-address not specified"))
   (function
    :reader call-frame-function
    :initarg :function
    :initform (error "function not specified"))))

(defun make-call-frame (closure return-address)
  (let ((number-of-registers (runtime.value:closure-registers-used closure)))
    (make-instance 'call-frame
                   :function closure
                   :registers (make-registers number-of-registers)
                   :return-address return-address)))

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
    :type (vector call-frame *))
   (size
    :initarg :size
    :type a:array-index)))

(defun make-call-stack (size)
  (make-instance 'call-stack
                 :size size
                 :frames (make-array size :element-type 'call-frame :initial-element nil :adjustable t)))

(defun call-stack-reset (call-stack)
  (with-slots (stack-pointer frames) call-stack
    (setf stack-pointer 0)
    (fill frames nil)))

(defun call-stack-push (call-stack call-frame)
  (restart-case (%call-stack-push call-stack call-frame)
    (stack-overflow-error ()
      :report "Increase call stack size to allow 10 more frames and continue"
      (increase-call-stack-size call-stack 10)
      (call-stack-push call-stack call-frame))))

(defun %call-stack-push (call-stack call-frame)
  (with-slots (stack-pointer frames size) call-stack
    (when (>= stack-pointer size)
      (error 'stack-overflow-error :call-stack call-stack))
    (setf (aref frames stack-pointer) call-frame)
    (incf stack-pointer)))

(defun increase-call-stack-size (call-stack amount)
  (with-slots (frames size) call-stack
    (let ((new-size (+ size amount)))
      (adjust-array frames new-size)
      (setf size new-size))))

(defun call-stack-pop (call-stack)
  (with-slots (stack-pointer frames) call-stack
    (decf stack-pointer)
    (let ((call-frame (aref frames stack-pointer)))
      (setf (aref frames stack-pointer) nil)
      call-frame)))

(defun call-stack-top (call-stack)
  (when (call-stack-empty-p call-stack)
    (error "Call stack is empty"))
  (with-slots (stack-pointer frames) call-stack
    (aref frames (1- stack-pointer))))

(defun call-stack-empty-p (call-stack)
  (with-slots (stack-pointer) call-stack
    (zerop stack-pointer)))

(defun call-stack-size (call-stack)
  (with-slots (stack-pointer) call-stack
    stack-pointer))
