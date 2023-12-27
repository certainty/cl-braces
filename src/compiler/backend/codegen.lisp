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
  ((current-package-name
    :initform nil
    :type (or null string)
    :documentation "The name of the package we're currently compiling")
   (symbol-table
    :initarg :symbol-table
    :initform (error "No symbol table specified")
    :type symbols:symbol-table
    :documentation "The symbol table we're using to resolve symbols")
   (variable-registers
    :initform (make-hash-table)
    :documentation "Map variable ids to the register they're stored in")
   (constants
    :initform (make-constants-builder)
    :type constants-builder
    :documentation "The constants builder we're using to build the constants table")
   (instructions
    :initform (make-array 0 :adjustable t :fill-pointer t :element-type 'bytecode:instruction)
    :type (vector bytecode:instruction)
    :documentation "The instructions we're generating")
   (functions
    :initform (make-array 0 :adjustable t :fill-pointer t :element-type 'bytecode:function-record)
    :type (vector bytecode:function-record *)
    :documentation "The function records which contain the function address and some metadata required by the runtime")
   (patch-function-calls
    :initform (make-hash-table :test #'equalp)
    :type hash-table
    :documentation "Map function names to the instruction that needs to patched with the address of the function")
   (blocklabels
    :initform (make-hash-table :test #'equalp)
    :type hash-table
    :documentation "Map label names to their addresses")
   (current-scope
    :initform 0
    :type scope-t
    :documentation "The current scope we're in")
   (entrypoint
    :initform 0
    :type bytecode:address-t
    :documentation "The address of the entrypoint of the chunk")
   (register-allocators
    :initform (list (make-register-allocator))
    :documentation "The stack of register allocators, that we're using to manage registers in functions.")))

(defun make-bytecode-generator (symbol-table)
  (make-instance 'bytecode-generator :symbol-table symbol-table))

(defmethod next-register ((generator bytecode-generator))
  (with-slots (register-allocators) generator
    (assert register-allocators)
    (let ((current-allocator (first register-allocators)))
      (next-register current-allocator))))

(defmethod registers-used ((generator bytecode-generator))
  (with-slots (register-allocators) generator
    (assert register-allocators)
    (let ((current-allocator (first register-allocators)))
      (registers-used current-allocator))))

(defun push-register-allocator (generator)
  "Pushes a new register allocator onto the stack, which will be used in subsequent calls to `next-register' and `registers-used'."
  (with-slots (register-allocators) generator
    (push (make-register-allocator) register-allocators)))

(defun pop-register-allocator (generator)
  "Pops the current register allocator from the stack and returns the number of registers used by it."
  (prog1 (registers-used generator)
    (pop (slot-value generator 'register-allocators))))

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

(defun add-constant (generator value)
  (with-slots (constants) generator
    (constants-add constants value)))

(defun add-instructions (generator &rest provided-instructions)
  (with-slots (instructions) generator
    (loop :for instr :in provided-instructions
          :do (vector-push-extend instr instructions))
    (1- (length instructions))))

(defun create-label (generator prefix)
  (with-slots (blocklabels instructions) generator
    (s:lret* ((name (gensym prefix))
              (addr (length instructions))
              (label (bytecode:label addr)))
      (setf (gethash name blocklabels) label)
      (values label addr))))

(defun generate-chunk (ast symbol-table)
  (let ((generator (make-bytecode-generator symbol-table)))
    (generate generator ast)
    (with-slots (instructions constants functions entrypoint blocklabels) generator
      (let ((registers-used (pop-register-allocator generator)))
        (make-instance 'bytecode:chunk
                       :constants (constants-result constants)
                       :functions functions
                       :code instructions
                       :entrypoint entrypoint
                       :block-labels blocklabels
                       :registers-used registers-used)))))

;; At a later point, when we only generate from simplified code in SSA we can switch to using ast:walk.
;; For now we implement the traversal ourselves to have full control
(defgeneric generate (generator node)
  (:documentation "Generate code for a node"))

(defmethod generate ((generator bytecode-generator) (node ast:function-declaration))
  (with-slots (symbol-table constants) generator
    (let* ((signature (ast:function-declaration-signature node))
           (name (ast:identifier-name (ast:function-declaration-name signature)))
           (params (ast:parameter-list-parameters (ast:function-signature-parameters signature)))
           (identifier-lists (mapcar #'ast:parameter-declaration-identifiers params))
           (body (ast:function-declaration-body node)))
      (multiple-value-bind (function-label function-addresss) (create-label generator name)
        (enter-scope generator)
        (push-register-allocator generator)
        (dolist (identifier-list identifier-lists)
          (registers-for-identifiers function-generator identifier-list :create-if-missing t))
        (generate generator body)
        (leave-scope generator)
        (let ((registers-used (pop-register-allocator generator))
              (record (bytecode:function-record :name name
                                                :address function-address
                                                :arity (bytecode:arity-exactly (length identifier-lists)))))
          (functions-add generator record name)
          function-label)))))

(defun functions-add (generator function-record name)
  (with-slots (functions symbol-table) generator
    (setf (gethash name functions) function-record)
    (symbols:add-symbol symbol-table name :function)))

(defmethod generate ((generator bytecode-generator) (node ast:node))
  (declare (ignore node generator))
  t)

(defmethod generate ((generator bytecode-generator) (node ast:source-file))
  (dolist (decl (ast:source-file-declarations node))
    (generate generator decl)))

(defmethod generate ((generator bytecode-generator) (node ast:statement-list))
  (let ((statements (ast:statement-list-statements node)))
    (loop for statement in statements
          for stmt-reg = (generate generator statement)
          finally (return stmt-reg))))

(defmethod generate ((generator bytecode-generator) (node ast:expression-list))
  (let ((expressions (ast:expression-list-expressions node)))
    (loop for expression in expressions
          for expr-reg = (generate generator expression)
          finally (return expr-reg))))

(defmethod generate ((generator bytecode-generator) (node ast:expression-statement))
  (let ((expression (ast:expression-statement-expression node)))
    (generate generator expression)))

(defmethod generate ((generator bytecode-generator) (node ast:grouping-expression))
  (generate generator (ast:grouping-expression-expression node)))

(defmethod generate ((generator bytecode-generator) (node ast:literal))
  (s:lret* ((const-address (add-constant generator (runtime.value:box (ast:literal-value node))))
            (register (next-register generator)))
    (add-instructions generator (bytecode:instr 'bytecode:const register const-address))))

(defmethod generate ((generator bytecode-generator) (node ast:unary-expression))
  (s:lret ((op (ast:unary-expression-operator node))
           (operand (generate generator (ast:unary-expression-operand node))))
    (assert operand)
    (cond
      ((token:class= op token:@MINUS)
       (add-instructions generator (bytecode:instr 'bytecode:neg operand)))
      ((token:class= op token:@PLUS) t)
      (t (todo! "unary operator")))))

(defmethod generate ((generator bytecode-generator) (node ast:binary-expression))
  (let* ((op (ast:binary-expression-operator node))
         (left  (generate generator (ast:binary-expression-lhs node)))
         (right (generate generator (ast:binary-expression-rhs node))))
    (assert left)
    (assert right)
    (prog1 left
      (cond
        ((token:class= op token:@PLUS)
         (add-instructions generator (bytecode:instr 'bytecode:add left right)))
        ((token:class= op token:@MINUS)
         (add-instructions generator (bytecode:instr 'bytecode:sub left right)))
        ((token:class= op token:@STAR)
         (add-instructions generator (bytecode:instr 'bytecode:mul left right)))
        ((token:class= op token:@SLASH)
         (add-instructions generator (bytecode:instr 'bytecode:div left right)))
        (t (todo! "binary operator"))))))

(defmethod generate ((generator bytecode-generator) (node ast:variable-declaration))
  (loop for spec in (ast:variable-declaration-specifications node)
        for reg = (generate generator spec)
        finally (return reg)))

(defmethod generate ((generator bytecode-generator) (node ast:variable-specification))
  (let* ((identifier-list (ast:variable-specification-identifiers node))
         (typespec (ast:variable-specification-type node))
         (expression-list (ast:variable-specification-initializer node))
         (registers (registers-for-identifiers generator identifier-list :create-if-missing t)))

    (if (null expression-list)
        ;; initialize to the zero value
        (loop for reg in registers
              do (add-instructions generator (bytecode:instr 'bytecode:const reg (add-constant generator (zero-value-for typespec))))
              finally (return reg))

        (loop for src-reg in (generate generator expression-list)
              for dst-reg in registers
              do (add-instructions generator (bytecode:instr 'bytecode:mov dst-reg src-reg))
              finally (return dst-reg)))))

(defun zero-value-for (typespec)
  (let ((name (token:lexeme (ast:type-specifier-name typespec))))
    (cond
      ((string= name "int") (runtime.value:box 0))
      ((string= name "bool") (runtime.value:box nil))
      (t (error "No zero value for type ~A" name)))))

(defmethod generate ((generator bytecode-generator) (node ast:short-variable-declaration))
  (let* ((expression-list (ast:short-variable-declaration-expressions node))
         (identifier-list (ast:short-variable-declaration-identifiers node)))
    (loop for src-reg in (generate generator expression-list)
          for dst-reg in (registers-for-identifiers generator identifier-list :create-if-missing t)
          do (add-instructions generator (bytecode:instr 'bytecode:mov dst-reg src-reg))
          finally (return dst-reg))))

(defmethod generate ((generator bytecode-generator) (node ast:expression-list))
  (let ((expressions (ast:expression-list-expressions node)))
    (loop for expression in expressions
          for reg = (generate generator expression)
          collect reg)))

(defun registers-for-identifiers (generator identifier-list &key (create-if-missing nil))
  (with-slots (variable-registers) generator
    (loop for identifier in (ast:identifier-list-identifiers identifier-list)
          for id = (find-scoped-variable generator (ast:identifier-name identifier))
          for reg = (find-register-for generator id)
          do (when (and create-if-missing (null reg))
               (setf reg (create-register-for generator id)))
          collect reg)))

(defmethod generate ((generator bytecode-generator) (node ast:identifier))
  (let ((name (ast:identifier-name node)))
    (a:when-let ((variable-id (find-scoped-variable generator name)))
      (find-register-for generator variable-id))))

(defmethod generate ((generator bytecode-generator) (node ast:empty-statement))
  (declare (ignore node generator))
  t)

(defmethod generate ((generator bytecode-generator) (node ast:if-statement))
  (let* ((init-stmt (ast:if-statement-init node))
         (condition (ast:if-statement-condition node))
         (condition-reg nil)
         (consequence (ast:if-statement-consequence node))
         (alternative (ast:if-statement-alternative node))
         (jz-addr 0)
         (cons-jmp-addr 0)
         (label-alternative nil)
         (label-end nil)
         (patch-this-address (bytecode:label #xDEADBEEF)))

    (generate generator init-stmt)
    (setf condition-reg (generate generator condition))

    (add-instructions generator (bytecode:instr 'bytecode:test condition-reg))
    (setf jz-addr (add-instructions generator (bytecode:instr 'bytecode:jz patch-this-address condition-reg)))

    (generate generator consequence)
    (setf cons-jmp-addr (add-instructions generator (bytecode:instr 'bytecode:jmp patch-this-address)))

    (when alternative
      (setf label-alternative (create-label generator "__else"))
      (generate generator alternative))

    (setf label-end (create-label generator "__end"))

    (if alternative
        (replace-instruction generator jz-addr (bytecode:instr 'bytecode:jz label-alternative condition-reg))
        (replace-instruction generator jz-addr (bytecode:instr 'bytecode:jz label-end condition-reg)))

    (replace-instruction generator cons-jmp-addr (bytecode:instr 'bytecode:jmp label-end))))

(defun patch-instruction (generator address instruction)
  (with-slots (instructions) generator
    (setf (aref instructions address) instruction)))

(defmethod generate ((generator bytecode-generator) (node ast:block))
  (enter-scope generator)
  (let ((statements (ast:block-statements node)))
    (generate generator statements))
  (leave-scope generator))
