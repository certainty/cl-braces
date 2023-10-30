RUN_LISP=sbcl --noinform --non-interactive
LISP=sbcl --noinform

test:
	$(RUN_LISP) --eval '(progn (ql:quickload :cl-braces/compiler/tests) (asdf:test-system :cl-braces/compiler))'

repl:
	$(LISP)	--eval '(ql:quickload :cl-braces)'

build:
	$(RUN_LISP) --eval '(asdf:make :cl-braces)'

deps:
	$(RUN_LISP) --eval '(progn (ql:quickload :cl-braces) (uiop:quit))'

.PHONY: test build repl deps
