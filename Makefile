.PHONY: test install-deps update-deps bootstrap-dev-env

test: install-deps
     qlot exec ros run -e "(progn (asdf:load-system 'cl-braces) (asdf:test-system 'cl-braces))"

install-deps:
	qlot install

update-deps:
	qlot update

bootstrap-dev-env:
	ros install sbcl
	ros install fukamachi/qlot
