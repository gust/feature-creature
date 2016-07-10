all: build

build:
	stack build --copy-bins

build-clean:
	stack clean && stack build --copy-bins

.PHONY: test
test:
	stack build --test

