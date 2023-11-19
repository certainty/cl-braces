(in-package :cl-braces.compiler.backend.codegen)
;;;; Code generation
;;;; TODO: explain how this works

;;; Register allocation
;;; TODO: explain how this works
;;;
(defclass register-allocator ()
  ((next-register
    :initform -1)))

(defgeneric next-register (allocator)
  (:documentation "Get the next register to use"))

(defmethod next-register ((allocator register-allocator))
  (with-slots (next-register) allocator
    (bytecode:register (incf next-register))))

(defmethod registers-used ((allocator register-allocator))
  (with-slots (next-register) allocator
    (1+ next-register)))

(defun make-register-allocator ()
  (make-instance 'register-allocator))

(defclass bytecode-generator ()
  ((symbol-table
    :initarg :symbol-table
    :initform (error "No symbol table specified"))
   (chunk-builder
    :initform (make-chunk-builder))
   (variable-registers
    :initform (make-hash-table))
   (current-scope
    :initform 0
    :type scope-t)
   (operand-stack
    :initform nil)
   (register-allocator
    :initform (make-register-allocator))))

(defun make-bytecode-generator (symbol-table)
  (make-instance 'bytecode-generator :symbol-table symbol-table))

(defun enter-scope (generator)
  (with-slots (current-scope) generator
    (incf current-scope)))

(defun leave-scope (generator)
  (with-slots (current-scope) generator
    (decf current-scope)))

(defun find-scoped-variable (generator name)
  (with-slots (current-scope variable-registers symbol-table) generator
    (a:when-let ((var (symbols:closest-scope current-scope (symbols:find-by-name symbol-table name :denotation #'symbols:denotes-variable-p))))
      (symbols:id var))))

(defun find-register-for (generator variable-id)
  (with-slots (variable-registers) generator
    (gethash variable-id variable-registers)))

(defun create-register-for (generator variable-id)
  (with-slots (variable-registers register-allocator) generator
    (let ((reg (next-register register-allocator)))
      (prog1 reg
        (setf (gethash variable-id variable-registers) reg)))))

(defun generate-chunk (ast symbol-table)
  (let ((generator (make-bytecode-generator symbol-table)))
    (ast:walk generator ast)
    (with-slots (chunk-builder register-allocator) generator
      (chunk-result chunk-builder (registers-used register-allocator)))))

(defmethod ast:enter ((generator bytecode-generator) (node ast:node))
  :continue)

(defmethod ast:leave ((generator bytecode-generator) (node ast:node))
  :continue)

(defmethod ast:enter ((generator bytecode-generator) (node ast:literal))
  (with-slots (chunk-builder register-allocator operand-stack) generator
    (let ((const-address (add-constant chunk-builder (value:box (ast:literal-value node))))
          (register (next-register register-allocator)))
      (push register operand-stack)
      (add-instructions chunk-builder (bytecode:instr 'bytecode:loada register const-address)))))

(defmethod ast:leave ((generator bytecode-generator) (node ast:unary-expression))
  (with-slots (operand-stack chunk-builder) generator
    (let ((op (ast:unary-expression-operator node))
          (operand (pop operand-stack)))
      (assert operand)
      (push operand operand-stack)
      (cond
        ((token:class= op token:@MINUS)
         (add-instructions chunk-builder (bytecode:instr 'bytecode:neg operand)))
        ((token:class= op token:@PLUS) nil)
        (t (todo! "unary operator"))))))

(defmethod ast:leave ((generator bytecode-generator) (node ast:binary-expression))
  (with-slots (operand-stack chunk-builder) generator
    (let ((op (ast:binary-expression-operator node))
          (right (pop operand-stack))
          (left (pop operand-stack)))
      (assert left)
      (assert right)
      (with-slots (register-allocator chunk-builder) generator
        (let ((dst (next-register register-allocator)))
          (push dst operand-stack)
          (cond
            ((token:class= op token:@PLUS)
             (add-instructions chunk-builder (bytecode:instr 'bytecode:add dst left right)))
            ((token:class= op token:@MINUS)
             (add-instructions chunk-builder (bytecode:instr 'bytecode:sub dst left right)))
            ((token:class= op token:@STAR)
             (add-instructions chunk-builder (bytecode:instr 'bytecode:mul dst left right)))
            ((token:class= op token:@SLASH)
             (add-instructions chunk-builder (bytecode:instr 'bytecode:div dst left right)))
            (t (todo! "binary operator"))))))))


(defmethod pop-n ((generator bytecode-generator) (n integer))
  (with-slots (operand-stack) generator
    (loop repeat n collect (pop operand-stack))))

;; TODO: we need to support multiple values
;; the operand stack has all the value registers on top
;; and then all the variable registers
(defmethod ast:leave ((generator bytecode-generator) (node ast:short-variable-declaration))
  (with-slots (operand-stack chunk-builder) generator
    (let* ((identifier-list (ast:short-variable-declaration-identifiers node))
           (number-of-variables (length (ast:identifier-list-identifiers identifier-list)))
           (value-regs  (pop-n generator number-of-variables))
           (variable-regs (reverse (pop-n generator number-of-variables))))

      (loop for identifier in (ast:identifier-list-identifiers identifier-list)
            for val in value-regs
            for var in variable-regs
            for var-id = (find-scoped-variable generator (ast:identifier-name identifier))
            do
               (assert var-id)
               (let ((var-reg (or var (create-register-for generator var-id))))
                 (push var-reg operand-stack)
                 (add-instructions chunk-builder (bytecode:instr 'bytecode:mov var-reg val)))))))

(defmethod ast:enter ((generator bytecode-generator) (node ast:identifier))
  (with-slots (operand-stack) generator
    (let ((name (ast:identifier-name node)))
      (a:when-let ((variable-id (find-scoped-variable generator name)))
        (let ((variable-reg (find-register-for generator variable-id)))
          ;; when we're in the context of a declaration, the definition is allowed to be null
          ;; should we set the generator state to indicate this and raise and error otherwise?
          (push variable-reg operand-stack))))))
