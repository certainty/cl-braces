(in-package :cl-braces.compiler.introspection)

(deftype tpe-compilation-phase ()
  '(member :scan :parse :typecheck :lower :optimize :codegen))

(define-condition introspection-event () ()
  (:documentation "A condition that is signaled at introspection points"))

(define-condition phase-event (introspection-event)
  ((phase :type tpe-compilation-phase :initarg :phase :reader compiler-phase-event-phase)))

(define-condition enter-phase (phase-event) ()
  (:documentation "A condition that is signaled at the beginning of each compiler phase"))

(define-condition leave-phase (phase-event) ()
  (:documentation "A condition that is signaled at the end of each compiler phase"))

(defgeneric emit (event)
  (:documentation "Emit an introspection event"))

(defmethod emit ((event introspection-event))
  (signal event))

(defmacro mark-phase (phase &body body)
  (let ((p (gensym)))
    `(let ((,p ,phase))
       (emit (make-instance 'enter-phase :phase ,p))
       (unwind-protect
            (progn ,@body)
         (emit (make-instance 'leave-phase :phase ,p))))))
