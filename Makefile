RELEASE_APP      ?= restyled-io-staging
RELEASE_REGISTRY ?= registry.heroku.com/$(RELEASE_APP)

.PHONY: restyled.io
restyled.io:
	docker build --tag restyled.io/restyled.io .

.PHONY: release
release: restyled.io
	docker tag restyled.io/restyled.io $(RELEASE_REGISTRY)/web
	docker push $(RELEASE_REGISTRY)/web
