RUN_LISP=sbcl --noinform --non-interactive
LISP=sbcl --noinform

test-vm:
	$(RUN_LISP) --eval '(progn (ql:quickload :cl-braces/tests) (asdf:test-system :cl-braces/vm))'

test-compiler:
	$(RUN_LISP) --eval '(progn (ql:quickload :cl-braces/tests) (asdf:test-system :cl-braces/compiler))'

repl:
	$(LISP)	--eval '(ql:quickload :cl-braces)'

build:
	$(RUN_LISP) --eval '(asdf:make :cl-braces)'

deps:
	$(RUN_LISP) --eval '(progn (ql:quickload :cl-braces) (uiop:quit))'

.PHONY: test build repl deps
