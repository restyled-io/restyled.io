# https://stackoverflow.com/questions/19232784/how-to-correctly-escape-sign-when-using-pattern-rules-and-patsubst-in-gnu-ma
PERCENT = %

all: setup setup.lint setup.tools build lint test

.PHONY: db.drop
db.drop:
	PGPASSWORD=password dropdb --user postgres --host localhost restyled
	PGPASSWORD=password dropdb --user postgres --host localhost restyled_test

.PHONY: db.create
db.create:
	PGPASSWORD=password createdb --user postgres --host localhost restyled
	PGPASSWORD=password createdb --user postgres --host localhost restyled_test

.PHONY: db.migrate
db.migrate:
	db/migrate dev upgrade
	db/migrate test upgrade

# N.B. db.seed clears seeded tables
.PHONY: db.seed
db.seed:
	PGPASSWORD=password psql --user postgres --host localhost restyled < db/seeds.sql

.PHONY: db.reset
db.reset: db.drop db.create db.migrate db.seed

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	stack build $(STACK_ARGUMENTS) --dependencies-only --test --no-run-tests

.PHONY: setup.lint
setup.lint:
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool \
	  hlint \
	  weeder

.PHONY: setup.tools
setup.tools:
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool \
	  brittany \
	  fast-tags \
	  stylish-haskell

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) --pedantic --test --no-run-tests

.PHONY: lint
lint:
	stack exec $(STACK_ARGUMENTS) hlint app src test
	stack exec $(STACK_ARGUMENTS) weeder .

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) --test

.PHONY: watch
watch:
	stack build $(STACK_ARGUMENTS) \
	  --fast --pedantic --test --file-watch \
	  --exec 'sh -c "pkill restyled.io; stack exec restyled.io &"' \
	  --ghc-options -DDEVELOPMENT


.PHONY: config/revision
config/revision:
	printf "$(PERCENT)s - $(PERCENT)s\n" \
	  "$$(git rev-parse HEAD)" \
	  "$$(git log HEAD -1 --format="$(PERCENT)cd")" \
	  > config/revision

Dockerfile.web: Dockerfile.in
	m4 -DAPPCMD=/app/restyled.io $^ > $@

Dockerfile.backend: Dockerfile.in
	m4 -DAPPCMD=/app/restyled.io-backend $^ > $@

.PHONY: dockerfiles
dockerfiles: Dockerfile.web Dockerfile.backend

# To release is just to tag. Quay.io will pick it up and build the image. Two
# tags are created because we need to build two images, one for each process,
# because Heroku provides no way to override CMD.
.PHONY: release
release:
	[ -n "$(VERSION)" ]
	git tag -a -m "v$(VERSION)" "v$(VERSION)"
	git tag -a -m "backend-v$(VERSION)" "backend-v$(VERSION)"
	git push --tags
