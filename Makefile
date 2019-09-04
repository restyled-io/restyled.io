# Bring system up to date after a pull
update: setup setup.lint setup.tools db.migrate build lint test

# Initialize from a fresh clone
bootstrap: setup setup.lint setup.tools db.setup build lint test db.seed

.PHONY: db.drop
db.drop:
	PGPASSWORD=password dropdb --if-exists --user postgres --host localhost restyled
	PGPASSWORD=password dropdb --if-exists --user postgres --host localhost restyled_test

.PHONY: db.create
db.create:
	PGPASSWORD=password createdb --user postgres --host localhost restyled
	PGPASSWORD=password createdb --user postgres --host localhost restyled_test

.PHONY: db.migrate
db.migrate:
	db/migrate dev upgrade
	db/migrate test upgrade

.PHONY: db.seed
db.seed:
	./bin/restyled.io-dev seed-db

.PHONY: db.setup
db.setup: db.create db.migrate

.PHONY: db.reset
db.reset: db.drop db.setup

.PHONY: db.console
db.console:
	PGHOST=localhost PGUSER=postgres PGPASSWORD=password psql restyled

.PHONY: db.console.prod
db.console.prod:
	heroku pg:psql --app restyled-io

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	# Avoid ExitFailure (-9) (THIS MAY INDICATE OUT OF MEMORY)
	stack build $(STACK_ARGUMENTS) -j 1 Cabal
	stack build $(STACK_ARGUMENTS) --dependencies-only --test --no-run-tests
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool \
	  dbmigrations-postgresql

.PHONY: setup.lint
setup.lint:
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool \
	  hlint \
	  weeder

.PHONY: setup.tools
setup.tools:
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool \
	  fast-tags \
	  stylish-haskell
	@# Need to install brittany from an old resolver and copy it into the
	@# current resolver's compiler-bin
	stack --resolver lts-12.26 build --copy-compiler-tool brittany
	ln -sf \
	  "$$(stack --resolver lts-12.26 path --compiler-tools-bin)"/brittany \
	  "$$(stack path --compiler-tools-bin)"/brittany

.PHONY: setup.ngrok
setup.ngrok:
	if ! command -v ngrok; then \
	  aurget -S ngrok; \
	  ngrok authtoken $$(pass ngrok/authtoken); \
	fi

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) --pedantic --test --no-run-tests

.PHONY: lint
lint:
	stack exec $(STACK_ARGUMENTS) hlint app src test
	# Weeder doesn't work with stack-2.0 :(
	# stack exec $(STACK_ARGUMENTS) weeder .

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) --test

.PHONY: watch
watch:
	stack build $(STACK_ARGUMENTS) \
	  --fast --pedantic --test --file-watch \
	  --exec 'sh -c "pkill restyled.io; bin/restyled.io-dev web & bin/restyled.io-dev backend &"' \
	  --ghc-options -DDEVELOPMENT

.PHONY: ngrok.http
ngrok.http: setup.ngrok
	ngrok http -subdomain restyled 3000
