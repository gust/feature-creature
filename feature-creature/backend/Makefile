all: build

build: elm
	stack build --copy-bins

.PHONY: elm
elm:
	cd ../frontend && \
		elm make src/Main.elm --warn --output ../backend/public/elm.js && \
		cd ../backend

build-clean:
	stack clean && stack build --copy-bins

.PHONY: test
test:
	stack build --test

