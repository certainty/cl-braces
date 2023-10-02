LISP=sbcl
CI_LISP=sbcl --noinform --no-userinit --no-sysinit --non-interactive

test:
	$(LISP) --non-interactive --eval '(asdf:test-system :cl-braces)'

repl:
	$(LISP)	--eval '(ql:quickload :cl-braces)'

build:
	$(LISP) --eval '(asdf:make :cl-braces)'

.PHONY: test repl
