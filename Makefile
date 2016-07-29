all: build

build:
	cd users-service-client && make build && cd - && \
	cd users-service && make build && cd - && \
	cd auth-service && make build && cd - && \
	cd marketing-site && make build && cd - && \
	cd features-worker && make build && cd - && \
	cd feature-creature/backend && make build && cd -

build-clean:
	cd users-service-client && make build-clean && cd - && \
	cd users-service && make build-clean && cd - && \
	cd auth-service && make build-clean && cd - && \
	cd marketing-site && make build-clean && cd - && \
	cd features-worker && make build-clean && cd - && \
	cd feature-creature/backend && make build-clean && cd -

.PHONY: test
test:
	cd users-service-client && make test && cd - && \
	cd users-service && make test && cd - && \
	cd auth-service && make test && cd - && \
	cd marketing-site && make test && cd - && \
	cd features-worker && make test && cd - && \
	cd feature-creature/backend && make test && cd -

setupdb:
	./users-service/scripts/setup-database.sh; \
	./feature-creature/backend/scripts/setup-database.sh

server:
	make build

	mkdir -p $(HOME)/.fc-features-worker
	cp features-worker/env/development.env $(HOME)/.fc-features-worker

	mkdir -p $(HOME)/.fc-marketing-site
	cp marketing-site/env/development.env $(HOME)/.fc-marketing-site
	cp -R marketing-site/public $(HOME)/.fc-marketing-site
	
	mkdir -p $(HOME)/.fc-auth-service
	cp auth-service/env/development.env $(HOME)/.fc-auth-service
	
	mkdir -p $(HOME)/.fc-users-service
	cp users-service/env/development.env $(HOME)/.fc-users-service
	
	mkdir -p $(HOME)/.fc-feature-creature
	cp feature-creature/backend/env/development.env $(HOME)/.fc-feature-creature
	cp -R feature-creature/backend/public $(HOME)/.fc-feature-creature
	
	foreman start -f Procfile.local
