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
    :initform (make-hash-table)
    :type (hash-table)
    :documentation "The function records which contain the function address and some metadata required by the runtime")
   (patch-function-calls
    :initform (make-hash-table :test #'equalp)
    :type hash-table
    :documentation "Map function names to the instruction that needs to patched with the address of the function")
   (block-labels
    :initform (make-hash-table :test #'equalp)
    :type hash-table
    :documentation "Map label names to their addresses")
   (current-scope
    :initform 0
    :type scope-t
    :documentation "The current scope we're in")
   (main-function
    :initform nil
    :type (or null bytecode:address-t)
    :documentation "The address of the main function if there is one")b
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
  (with-slots (current-scope variable-registers current-package-name symbol-table) generator
    (a:when-let ((var (symbols:closest-scope current-scope (symbols:find-by-name symbol-table current-package-name name :denotation #'symbols:denotes-variable-p))))
      (symbols:id var))))

(defun find-function (generator name)
  (with-slots (functions symbol-table current-package-name) generator
    (a:when-let ((fun (symbols:find-by-name symbol-table current-package-name name :denotation #'symbols:denotes-function-p :scope<= 0)))
      (symbols:id (car fun)))))

(defun create-register-for (generator variable-id)
  (with-slots (variable-registers) generator
    (let ((reg (next-register generator)))
      (prog1 reg
        (setf (gethash variable-id variable-registers) reg)))))

(defun find-register-for (generator variable-id)
  (with-slots (variable-registers) generator
    (gethash variable-id variable-registers)))

(defun add-constant (generator value)
  (with-slots (constants) generator
    (constants-add constants value)))

(defun add-instructions (generator &rest provided-instructions)
  (with-slots (instructions) generator
    (loop :for instr :in provided-instructions
          :do (vector-push-extend instr instructions))
    (1- (length instructions))))

(defun create-label (generator prefix)
  (with-slots (block-labels instructions) generator
    (let ((label-name (string (gensym (concatenate 'string prefix "_"))))
          (address (bytecode:label (length instructions))))
      (setf (gethash address block-labels) label-name)
      (values address label-name))))

(defun generate-chunk (ast symbol-table)
  (let ((generator (make-bytecode-generator symbol-table))
        (entrypoint nil))
    (generate generator ast)
    (with-slots (instructions constants main-function block-labels) generator
      (when main-function
        (setf entrypoint (generate-entrypoint generator)))
      (let ((registers-used (pop-register-allocator generator)))
        (make-instance 'bytecode:chunk
                       :constants (constants-result constants)
                       :code instructions
                       :entrypoint entrypoint
                       :block-labels block-labels
                       :registers-used registers-used)))))

(defun generate-entrypoint (generator)
  (with-slots (main-function) generator
    (push-register-allocator generator)
    (let ((main-function-reg (next-register generator)))
      (multiple-value-bind (entrypoint-address entrypoint-label) (create-label generator "__gobraces_entrypoint")
        (add-instructions generator (bytecode:instr 'bytecode:const main-function-reg main-function))
        (add-instructions generator (bytecode:instr 'bytecode:call main-function-reg (bytecode:immediate 0)))
        entrypoint-address))))

;; At a later point, when we only generate from simplified code in SSA we can switch to using ast:walk.
;; For now we implement the traversal ourselves to have full control
(defgeneric generate (generator node)
  (:documentation "Generate code for a node"))

(defmethod generate ((generator bytecode-generator) (node ast:node))
  (declare (ignore node generator))
  t)

(defmethod generate ((generator bytecode-generator) (node ast:source-file))
  (generate generator (ast:source-file-package node))
  (dolist (decl (ast:source-file-declarations node))
    (generate generator decl)))

(defmethod generate ((generator bytecode-generator) (node ast:package-declaration))
  (with-slots (current-package-name) generator
    (setf current-package-name (ast:identifier-name (ast:package-declaration-name node)))))

(defmethod generate ((generator bytecode-generator) (node ast:function-declaration))
  (with-slots (symbol-table main-function functions) generator
    (let* ((signature (ast:function-declaration-signature node))
           (name (ast:identifier-name (ast:function-declaration-name node)))
           (params (ast:parameter-list-parameters (ast:function-signature-parameters signature)))
           (identifier-lists (mapcar #'ast:parameter-declaration-identifiers params))
           (body (ast:function-declaration-body node)))
      (multiple-value-bind (function-address function-label) (create-label generator (mangle-name generator name))
        (enter-scope generator)
        (push-register-allocator generator)
        (dolist (identifier-list identifier-lists)
          (registers-for-identifiers generator identifier-list :create-if-missing t))
        (generate generator body)
        (leave-scope generator)
        (let* ((registers-used (pop-register-allocator generator))
               (function-value (runtime.value:make-closure
                                function-address
                                (runtime.value:arity-exactly (length identifier-lists))
                                registers-used))
               (const-addr (add-constant generator function-value))
               (function-id (find-function generator name)))
          (assert function-id)
          (when (main-function-p function-label)
            (setf main-function const-addr))
          (setf (gethash function-id functions) const-addr)
          function-label)))))

(defun mangle-name (generator name)
  (with-slots (current-package-name) generator
    (if current-package-name
        (concatenate 'string current-package-name "#" name)
        name)))

(defun main-function-p (name)
  (plusp (cl-ppcre:count-matches "main#main_.+" name)))

(defmethod generate ((generator bytecode-generator) (node ast:function-call))
  (let* ((function-register (generate generator (ast:function-call-function node)))
         (arguments (ast:function-call-arguments node))
         (argument-registers nil))
    (when arguments
      (setf argument-registers (multiple-value-list (generate generator arguments))))
    (add-instructions generator (bytecode:instr 'bytecode:call function-register (bytecode:immediate (length argument-registers))))))

(defmethod generate ((generator bytecode-generator) (node ast:statement-list))
  (let ((statements (ast:statement-list-statements node)))
    (loop for statement in statements
          for stmt-reg = (generate generator statement)
          finally (return stmt-reg))))

(defmethod generate ((generator bytecode-generator) (node ast:expression-list))
  (let ((expressions (ast:expression-list-expressions node)))
    (loop for expression in expressions
          collect (generate generator expression)
          finally (values-list expr-reg))))

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
           (operand (generate generator (ast:unary-expression-operand node)))
           (result (next-register generator)))
    (assert operand)
    (cond
      ((token:class= op token:@MINUS)
       (add-instructions generator (bytecode:instr 'bytecode:neg result operand))
       result)
      ((token:class= op token:@PLUS)
       operand)
      (t (todo! "unary operator")))))

(defmethod generate ((generator bytecode-generator) (node ast:binary-expression))
  (let* ((op (ast:binary-expression-operator node))
         (left  (generate generator (ast:binary-expression-lhs node)))
         (right (generate generator (ast:binary-expression-rhs node)))
         (result (next-register generator)))
    (assert left)
    (assert right)
    (prog1 result
      (cond
        ((token:class= op token:@PLUS)
         (add-instructions generator (bytecode:instr 'bytecode:add result left right)))
        ((token:class= op token:@MINUS)
         (add-instructions generator (bytecode:instr 'bytecode:sub result left right)))
        ((token:class= op token:@STAR)
         (add-instructions generator (bytecode:instr 'bytecode:mul result left right)))
        ((token:class= op token:@SLASH)
         (add-instructions generator (bytecode:instr 'bytecode:div result left right)))
        ((token:class= op token:@EQUAL_EQUAL)
         (add-instructions generator (bytecode:instr 'bytecode:eq result left right)))
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
      (return-from generate (find-register-for generator variable-id)))
    (a:when-let ((function-id (find-function generator name)))
      (with-slots (functions) generator
        (let ((function-object-address (gethash function-id functions))
              (reg (next-register generator)))
          (prog1 reg
            (add-instructions generator (bytecode:instr 'bytecode:const reg function-object-address))))))))

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

(defmethod generate ((generator bytecode-generator) (node ast:return-statement))
  (let ((expressions (ast:return-statement-expressions node)))
    (if expressions
        (let ((registers (multiple-value-list (generate generator expressions))))
          (add-instructions generator (bytecode:instr 'bytecode:ret (bytecode:immediate (length registers)))))
        (add-instructions generator (bytecode:instr 'bytecode:ret (bytecode:immediate 0))))))

(defun replace-instruction (generator address instruction)
  (with-slots (instructions) generator
    (setf (aref instructions address) instruction)))

(defun patch-instruction (generator address instruction)
  (with-slots (instructions) generator
    (setf (aref instructions address) instruction)))

(defmethod generate ((generator bytecode-generator) (node ast:block))
  (enter-scope generator)
  (let ((statements (ast:block-statements node)))
    (generate generator statements))
  (leave-scope generator))
