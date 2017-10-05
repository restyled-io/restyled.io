NAME        ?= restyled.io
LOCAL_IMAGE ?= $(NAME)/$(NAME)

RELEASE_APP      ?= restyled-io-staging
RELEASE_REGISTRY ?= registry.heroku.com
RELEASE_IMAGE    ?= $(RELEASE_REGISTRY)/$(RELEASE_APP)/web

DOCKER_USERNAME ?= x
DOCKER_PASSWORD ?= x

ALL_RESTYLERS         ?= $(wildcard restylers/*)
RESTYLER_IMAGE_PREFIX ?= restyled/restyler-

# https://stackoverflow.com/questions/19232784/how-to-correctly-escape-sign-when-using-pattern-rules-and-patsubst-in-gnu-ma
PERCENT = %

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
	stack test --test-arguments "$(SPEC_ARGS)"

.PHONY: config/revision
config/revision:
	printf "$(PERCENT)s - $(PERCENT)s\n" \
	  "$$(git rev-parse HEAD)" \
	  "$$(git log HEAD -1 --format="$(PERCENT)cd")" \
	  > config/revision

.PHONY: image.build
image.build: config/revision
	docker build --tag "$(LOCAL_IMAGE)" .
	@# cleanup, in case we're testing locally
	@$(RM) config/revision

.PHONY: image.release
image.release:
	@docker login \
	  --username "$(DOCKER_USERNAME)" \
	  --password "$(DOCKER_PASSWORD)" \
	  "$(RELEASE_REGISTRY)" || echo "docker login failed, release may fail."
	docker tag "$(LOCAL_IMAGE)" "$(RELEASE_IMAGE)"
	docker push "$(RELEASE_IMAGE)"

.PHONY: image.release.backend
image.release.backend: RELEASE_IMAGE=restyled/restyled
image.release.backend:
	docker tag "$(LOCAL_IMAGE)" "$(RELEASE_IMAGE)"
	docker push "$(RELEASE_IMAGE)"

.PHONY: restylers
restylers: $(ALL_RESTYLERS)
	@for r in $^; do \
	  (cd "$$r" && \
	    docker build --tag "$(RESTYLER_IMAGE_PREFIX)$$(basename "$$r")" .); \
	done

.PHONY: restylers.release
restylers.release: $(ALL_RESTYLERS)
	@for r in $^; do \
	  docker push "$(RESTYLER_IMAGE_PREFIX)$$(basename "$$r")"; \
	done
