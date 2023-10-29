(in-package :cl-braces.isa)

(defstruct (chunk (:conc-name chunk-) (:constructor chunk (provided-constants  &rest provided-instructions)))
  (constants provided-constants :type (vector t *) :read-only t)
  (instructions (make-array (length provided-instructions) :element-type 'instruction :initial-contents provided-instructions) :type (vector instruction *) :read-only t))
