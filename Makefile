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
	AWS_PROFILE=restyled ./bin/restyled.io-dev seed-db

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
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install --copy-compiler-tool \
	  dbmigrations-postgresql

.PHONY: setup.lint
setup.lint:
	stack install --copy-compiler-tool \
	  hlint \
	  weeder

.PHONY: setup.tools
setup.tools:
	stack install --copy-compiler-tool \
	  dhall \
	  brittany \
	  fast-tags \
	  stylish-haskell

.PHONY: setup.ngrok
setup.ngrok:
	if ! command -v ngrok; then \
	  aurget -S ngrok; \
	  ngrok authtoken $$(pass ngrok/authtoken); \
	fi

.PHONY: build
build:
	stack build --pedantic --test --no-run-tests

.PHONY: lint
lint:
	find app src test -name '*.hs' \
	  -not -name 'Foundation.hs' \
	  -exec stack exec hlint {} +
	stack exec weeder

.PHONY: test
test:
	stack build --test

.PHONY: watch
watch:
	AWS_PROFILE=restyled stack build \
	  --fast --pedantic --test --file-watch \
	  --exec bin/restyled-restart \
	  --ghc-options -DDEVELOPMENT

.PHONY: ngrok.http
ngrok.http: setup.ngrok
	ngrok http -subdomain restyled 3000

.PHONY: image
image:
	docker run -it --rm \
	  --volume /var/run/docker.sock:/var/run/docker.sock \
	  --volume "$(HOME)"/.docker/config.json:/root/.docker/config.json:ro \
	  --volume "$(PWD)":/build:ro \
	  --workdir /build \
	  restyled/ops:v5 docker-build-remote-cache \
	  restyled/restyled.io:testing \
	  --build-arg "REVISION=testing"

.PHONY: image.check
image.check: image
	docker run -it --rm --net=host \
	  --volume "$(PWD)"/.env.development:/app/.env:ro \
	  restyled/restyled.io:testing /app/restyled.io \
	  -e .env web
