NAME        ?= restyled.io
LOCAL_IMAGE ?= $(NAME)/$(NAME)

IMAGE_CACHE_DIR  ?= /caches
IMAGE_CACHE_NAME ?= $(IMAGE_CACHE_DIR)/$(NAME).tar

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

.PHONY: image.load
image.load:
	docker load --input "$(IMAGE_CACHE_NAME)"

.PHONY: image.build
image.build:
	# Annoyingly, we have to build and save the pre-stages distinctly in
	# order for the layer caching to work on subsequent builds.
	#
	# See https://github.com/moby/moby/issues/34715.
	#
	docker build \
	  --cache-from "$(LOCAL_IMAGE)-builder" \
	  --target builder \
	  --tag "$(LOCAL_IMAGE)-builder" .
	docker build \
	  --cache-from "$(LOCAL_IMAGE)" \
	  --cache-from "$(LOCAL_IMAGE)-builder" \
	  --tag "$(LOCAL_IMAGE)" .

.PHONY: image.save
image.save:
	mkdir -p "$(IMAGE_CACHE_DIR)"
	docker save --output "$(IMAGE_CACHE_NAME)" \
	  "$(LOCAL_IMAGE)" \
	  "$(LOCAL_IMAGE)-builder"

.PHONY: image.release
image.release:
	@docker login \
	  --username "$(DOCKER_USERNAME)" \
	  --password "$(DOCKER_PASSWORD)" \
	  "$(RELEASE_REGISTRY)" || echo "docker login failed, release may fail."
	docker tag "$(LOCAL_IMAGE)" "$(RELEASE_IMAGE)"
	docker push "$(RELEASE_IMAGE)"
