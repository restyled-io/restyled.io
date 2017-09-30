NAME        ?= restyled.io
LOCAL_IMAGE ?= $(NAME)/$(NAME)

RELEASE_APP      ?= restyled-io-staging
RELEASE_REGISTRY ?= registry.heroku.com
RELEASE_IMAGE    ?= $(RELEASE_REGISTRY)/$(RELEASE_APP)/web

all: resetdb setup build lint test

.PHONY: resetdb
resetdb:
	PGPASSWORD=password dropdb --user postgres --host localhost restyled || true
	PGPASSWORD=password dropdb --user postgres --host localhost restyled_test || true
	PGPASSWORD=password createdb --user postgres --host localhost restyled
	PGPASSWORD=password createdb --user postgres --host localhost restyled_test

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	stack build --pedantic --test --no-run-tests

.PHONY: lint
lint:
	hlint .
	weeder .

.PHONY: test
test:
	stack test


.PHONY: image.build
image.build:
	docker build --tag "$(LOCAL_IMAGE)" .

.PHONY: image.release
image.release:
	@docker login \
	  --username "$(DOCKER_USERNAME)" \
	  --password "$(DOCKER_PASSWORD)" \
	  "$(RELEASE_REGISTRY)" || echo "docker login failed, release may fail."
	docker tag "$(LOCAL_IMAGE)" "$(RELEASE_IMAGE)"
	docker push "$(RELEASE_IMAGE)"
