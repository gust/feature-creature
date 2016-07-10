all: build

build:
	cd users-service-client && make build && cd - && \
	cd users-service && make build && cd - && \
	cd auth-service && make build && cd - && \
	cd marketing-site && make build && cd -

build-clean:
	cd users-service-client && make build-clean && cd - && \
	cd users-service && make build-clean && cd - && \
	cd auth-service && make build-clean && cd - && \
	cd marketing-site && make build-clean && cd -

.PHONY: test
test:
	cd users-service-client && make test && cd - && \
	cd users-service && make test && cd - && \
	cd auth-service && make test && cd - && \
	cd marketing-site && make test && cd -

setupdb:
	./users-service/scripts/setup-database.sh

server:
	mkdir -p $(HOME)/.fc-marketing-site
	cp marketing-site/env/development.env $(HOME)/.fc-marketing-site
	
	mkdir -p $(HOME)/.fc-auth-service
	cp auth-service/env/development.env $(HOME)/.fc-auth-service
	
	mkdir -p $(HOME)/.fc-users-service
	cp users-service/env/development.env $(HOME)/.fc-users-service
	
	foreman start -f Procfile.local
