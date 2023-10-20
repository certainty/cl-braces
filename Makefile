RUN_LISP=sbcl --noinform --non-interactive
LISP=sbcl --noinform

test:
	$(RUN_LISP) --non-interactive --eval '(asdf:test-system :cl-braces/compiler)'

repl:
	$(LISP)	--eval '(ql:quickload :cl-braces)'

build:
	$(RUN_LISP) --eval '(asdf:make :cl-braces)'

.PHONY: test repl
