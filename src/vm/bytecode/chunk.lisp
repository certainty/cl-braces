(in-package :cl-braces.vm.bytecode)

(defclass constant-table ()
  ((table :reader :table :initform (error "must provide constants") :initarg :constants :type (vector runtime:value *))))

(defclass chunk ()
  ((constants :accessor :chunk-constants :initform (error "must provide constant table") :initarg :constants :type constant-table)
   (instructions :reader :chunk-instructions :initform (error "must provide instructions") :initarg :instructions :type (vector instruction *))))
