LOCAL_IMAGE   ?= restyled/restyled.io
RELEASE_IMAGE ?= $(LOCAL_IMAGE)

DOCKER_USERNAME ?= x
DOCKER_PASSWORD ?= x

# https://stackoverflow.com/questions/19232784/how-to-correctly-escape-sign-when-using-pattern-rules-and-patsubst-in-gnu-ma
PERCENT = %

all: build lint test

.PHONY: db.drop
db.drop:
	PGPASSWORD=password dropdb --user postgres --host localhost restyled
	PGPASSWORD=password dropdb --user postgres --host localhost restyled_test

.PHONY: db.create
db.create:
	PGPASSWORD=password createdb --user postgres --host localhost restyled
	PGPASSWORD=password createdb --user postgres --host localhost restyled_test

# N.B. db.seed clears seeded tables
.PHONY: db.seed
db.seed:
	PGPASSWORD=password psql --user postgres --host localhost restyled < seeds.sql

.PHONY: db.reset
db.reset: db.drop db.create

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install --copy-compiler-tool hlint weeder

.PHONY: build
build:
	stack build \
	  --fast --pedantic --test --no-run-tests

.PHONY: watch
watch:
	stack build \
	  --fast --pedantic --test --file-watch \
	  --exec 'sh -c "pkill restyled.io; stack exec restyled.io &"' \
	  --ghc-options -DDEVELOPMENT

.PHONY: lint
lint:
	stack exec hlint .
	stack exec weeder .

.PHONY: test
test:
	stack build --fast --pedantic --test

.PHONY: clean
clean:
	stack clean

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
	  --password "$(DOCKER_PASSWORD)" || \
	  echo "docker login failed, release may fail."
	docker tag "$(LOCAL_IMAGE)" "$(RELEASE_IMAGE)"
	docker push "$(RELEASE_IMAGE)"
