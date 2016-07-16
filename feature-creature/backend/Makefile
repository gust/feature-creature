all: build

build:
	stack build --copy-bins

.PHONY: elm
elm:
	cd elm && elm make src/Main.elm --warn --output ../public/elm.js && cd -

build-clean:
	stack clean && stack build --copy-bins

.PHONY: test
test:
	stack build --test

