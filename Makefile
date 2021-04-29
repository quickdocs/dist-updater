all: run

NETWORK := host
DB_HOST := localhost
DB_PORT := 5432
DB_NAME := quickdocs
DB_USERNAME := quickdocs
DB_PASSWORD := quickdocs

ifndef version
override version = $(shell curl -s -L http://storage.googleapis.com/quickdocs-dist/quicklisp/info.json | jq -r ".latest_version")
endif

.PHONY: build
build:
	docker build -t quickdocs-dist-updater -f Dockerfile .

.PHONY: release
release:
	docker build -t ghcr.io/quickdocs/dist-updater -f Dockerfile.release .

.PHONY: publish
publish:
	docker push ghcr.io/quickdocs/dist-updater

.PHONY: run
run:
	docker run --rm -it -v ${PWD}:/app --net=$(NETWORK) \
		-e DB_HOST=$(DB_HOST) -e DB_PORT=$(DB_PORT) \
		-e DB_NAME=$(DB_NAME) -e DB_USERNAME=$(DB_USERNAME) -e DB_PASSWORD=$(DB_PASSWORD) \
		quickdocs-dist-updater update $(version)

.PHONY: generate-migrations
generate-migrations:
	docker run --rm -it -v ${PWD}:/app --net=$(NETWORK) --entrypoint sbcl quickdocs-dist-updater \
		--noinform --non-interactive \
		--eval '(ql:quickload :dist-updater)' \
		--eval '(mito:connect-toplevel :postgres :database-name "$(DB_NAME)" :host "$(DB_HOST)" :port $(DB_PORT) :username "$(DB_USERNAME)" :password "$(DB_PASSWORD)")' \
		--eval '(mito:generate-migrations #P"db/")'

.PHONY: migrate
migrate:
	docker run --rm -it -v ${PWD}:/app --net=$(NETWORK) \
		-e DB_HOST=$(DB_HOST) -e DB_PORT=$(DB_PORT) \
		-e DB_NAME=$(DB_NAME) -e DB_USERNAME=$(DB_USERNAME) -e DB_PASSWORD=$(DB_PASSWORD) \
		quickdocs-dist-updater migrate
