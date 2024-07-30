# Bring system up to date after a pull
update: setup setup-tools db-migrate build lint test

# Initialize from a fresh clone
bootstrap: \
  setup \
  setup-tools \
  setup-ngrok \
  db-setup \
  build \
  lint \
  test \
  dotenv \
  db-seed

# Create .env.development from .env.example, pulling secrets in AWS SSM
dotenv:
  { ./bin/make-ssm-m4; cat .env.example; } | m4 .env.development

# Drop dev and test DBs
db-drop:
  PGPASSWORD=password dropdb --if-exists --user postgres --host localhost restyled
  PGPASSWORD=password dropdb --if-exists --user postgres --host localhost restyled_test

# Create dev and test DBs
db-create:
  PGPASSWORD=password createdb --user postgres --host localhost restyled
  PGPASSWORD=password createdb --user postgres --host localhost restyled_test

# Migrate dev and test DBs
db-migrate:
  db/migrate dev upgrade
  db/migrate test upgrade

# Seed dev and test DBs
db-seed:
  ./bin/restyled.io-dev seed-db

# Setup dev and test DBs
db-setup: db-create db-migrate

# Reset dev and test DBs
db-reset: db-drop db-setup

# Open a console against dev DB
db-console:
  PGHOST=localhost PGUSER=postgres PGPASSWORD=password psql restyled

# Open a console against Prod
db-console-prod:
  heroku pg:psql --app restyled-io

# Setup for Haskell development
setup:
  stack setup
  stack build --dependencies-only --test --no-run-tests

# Setup for Haskell linting
setup-tools:
  stack install --copy-compiler-tool \
    apply-refact \
    dhall \
    fast-tags \
    fourmolu \
    hlint \
    weeder

# Setup to use ngrok
setup-ngrok:
  ngrok authtoken $$(pass ngrok/authtoken)

# Build the project
build:
  stack build --pedantic --test --no-run-tests

# Lint
lint:
  stack exec -- hlint . --ignore-glob src/Restyled/Widgets/Job.hs
  stack exec -- weeder --require-hs-files
  stack lint-extra-deps

# Test the project
test:
  stack build --test

# Rebuild (and test) the project on file changes
watch:
  stack build \
    --fast --pedantic --test --file-watch \
    --exec bin/restyled-restart \
    --ghc-options -DDEVELOPMENT

# Rebuild the project on file changes
watch-no-test:
  stack build \
    --fast --pedantic --test --no-run-tests --file-watch \
    --exec bin/restyled-restart \
    --ghc-options -DDEVELOPMENT

# Run an ngrok process
ngrok-http:
  ngrok http --subdomain restyled 3000

# Build the docker image
image:
  docker build \
    --build-arg "REVISION=testing" \
    --tag restyled/restyled.io:testing \
    .

# Check the build docker image
image-check: image
  docker run -it --rm --net=host \
    --volume "$HOME"/.aws:/root/.aws:ro \
    --volume "$PWD"/.env.development:/app/.env:ro \
    restyled/restyled.io:testing /app/restyled.io \
    -e .env
