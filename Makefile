RUN_LISP=sbcl --noinform --non-interactive
LISP=sbcl --noinform

release_dependencies := serapeum alexandria frugal-uuid
dev_dependencies := lisp-unit2 sblint 40ants-critic

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

lint:
	~/.roswell/bin/sblint | reviewdog -efm="%f:%l:%c: %m" -diff="git diff main"

critic:
	ros -e '(ql:quickload :40ants-critic)' -e '(40ants-critic:critique-asdf-system :cl-braces)'

install_dependencies: setup_roswell install_dev_dependencies install_release_dependencies

install_release_dependencies:
	ros install $(release_dependencies)

install_dev_dependencies: install_reviewdog
	ros install $(dev_dependencies)

setup_roswell:
	ros -e '(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)'

install_reviewdog:
	go install github.com/reviewdog/reviewdog/cmd/reviewdog@latest

.PHONY: test build repl deps
