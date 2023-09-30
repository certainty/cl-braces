.PHONY: test install-deps update-deps bootstrap-dev-env

test: install-deps
	.qlot/bin/rove cl-braces.asd

install-deps:
	qlot install

update-deps:
	qlot update

bootstrap-dev-env:
	ros install sbcl
	ros install fukamachi/qlot
