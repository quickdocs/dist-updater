all: run

NETWORK := host
DB_HOST := localhost
DB_PORT := 5432
DB_NAME := quickdocs
DB_USERNAME := quickdocs
DB_PASSWORD := quickdocs

.PHONY: build
build:
	docker build -t quickdocs-dist-updater .

.PHONY: run
run:
	docker run --rm -it -v ${PWD}:/app --net=$(NETWORK) quickdocs-dist-updater \
		$(VERSION) \
		--host $(DB_HOST) --port $(DB_PORT) \
		--dbname $(DB_NAME) --username $(DB_USERNAME) --password $(DB_PASSWORD)

.PHONY: generate-migrations
generate-migrations:
	docker run --rm -it -v ${PWD}:/app --net=$(NETWORK) --entrypoint sbcl quickdocs-dist-updater \
		--noinform --non-interactive \
		--eval '(ql:quickload :dist-updater)' \
		--eval '(mito:connect-toplevel :postgres :database-name "$(DB_NAME)" :host "$(DB_HOST)" :port $(DB_PORT) :username "$(DB_USERNAME)" :password "$(DB_PASSWORD)")' \
		--eval '(mito:generate-migrations #P"db/")'

.PHONY: db
db:
	docker run --rm -it -v ${PWD}:/app --net=$(NETWORK) --entrypoint sbcl quickdocs-dist-updater \
		--noinform --non-interactive \
		--eval '(ql:quickload :dist-updater)' \
		--eval '(mito:connect-toplevel :postgres :database-name "$(DB_NAME)" :host "$(DB_HOST)" :port $(DB_PORT) :username "$(DB_USERNAME)" :password "$(DB_PASSWORD)")' \
		--eval '(mito:migrate #P"db/")'
