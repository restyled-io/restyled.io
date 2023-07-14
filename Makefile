# Bring system up to date after a pull
update: setup setup.tools db.migrate build lint test

# Initialize from a fresh clone
bootstrap: \
  setup \
  setup.tools \
  setup.ngrok \
  db.setup \
  build \
  lint \
  test \
  .env.development \
  db.seed

.env.development: .env.example
	{ ./bin/make-ssm-m4; cat $^; } | m4 >$@


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
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install --copy-compiler-tool \
	  dbmigrations-postgresql

.PHONY: setup.tools
setup.tools:
	stack install --copy-compiler-tool \
	  apply-refact \
	  dhall \
	  fast-tags \
	  hlint \
	  weeder-2.4.0
	@# install fourmolu-0.12 using a 9.6 resolver, then move it to the 9.2
	@# compiler-tools-bin
	stack \
	  --resolver nightly-2023-06-26 install \
	  --copy-compiler-tool fourmolu-0.13.0.0
	cp -v \
	  "$$(stack --resolver nightly-2023-06-26 path --compiler-tools-bin)/fourmolu" \
	  "$$(stack path --compiler-tools-bin)/fourmolu"

.PHONY: setup.ngrok
setup.ngrok:
	ngrok authtoken $$(pass ngrok/authtoken)

.PHONY: build
build:
	stack build --pedantic --test --no-run-tests

.PHONY: lint
lint:
	stack exec -- hlint . --ignore-glob src/Restyled/Widgets/Job.hs
	stack exec -- weeder --require-hs-files

.PHONY: test
test:
	stack build --test

.PHONY: watch
watch:
	stack build \
	  --fast --pedantic --test --file-watch \
	  --exec bin/restyled-restart \
	  --ghc-options -DDEVELOPMENT

.PHONY: ngrok.http
ngrok.http:
	ngrok http --subdomain restyled 3000

.PHONY: image
image:
	docker build \
	  --build-arg "REVISION=testing" \
	  --tag restyled/restyled.io:testing \
	  .

.PHONY: image.check
image.check: image
	docker run -it --rm --net=host \
	  --volume "$(HOME)"/.aws:/root/.aws:ro \
	  --volume "$(PWD)"/.env.development:/app/.env:ro \
	  restyled/restyled.io:testing /app/restyled.io \
	  -e .env
