.PHONY: image

image:
	docker build --tag restyled.io/restyled.io .

release:
	docker tag restyled.io/restyled.io registry.heroku.com/restyled-io/web
	docker push registry.heroku.com/restyled-io/web
