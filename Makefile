RELEASE_APP      ?= restyled-io-staging
RELEASE_REGISTRY ?= registry.heroku.com/$(RELEASE_APP)

.PHONY: resetdb
resetdb:
	PGPASSWORD=password dropdb --user postgres --host localhost restyled
	PGPASSWORD=password dropdb --user postgres --host localhost restyled_test
	PGPASSWORD=password createdb --user postgres --host localhost restyled
	PGPASSWORD=password createdb --user postgres --host localhost restyled_test

.PHONY: restyled.io
restyled.io:
	docker build --tag restyled.io/restyled.io .

.PHONY: release
release: restyled.io
	docker tag restyled.io/restyled.io $(RELEASE_REGISTRY)/web
	docker push $(RELEASE_REGISTRY)/web
