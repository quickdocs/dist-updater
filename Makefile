all: run

NETWORK := host
DB_HOST := localhost
DB_PORT := 5432
DB_NAME := quickdocs
DB_USERNAME := quickdocs
DB_PASSWORD := quickdocs

.PHONY: build
build:
	docker build -t dist-updater .

.PHONY: run
run:
	docker run --rm -it -v ${PWD}:/app --net=$(NETWORK) dist-updater \
		$(VERSION) \
		--host $(DB_HOST) --port $(DB_PORT) \
		--dbname $(DB_NAME) --username $(DB_USERNAME) --password $(DB_PASSWORD)

.PHONY: db
db:
	PGPASSWORD=$(DB_PASSWORD) psql -h $(DB_HOST) -p $(DB_PORT) -U $(DB_USERNAME) $(DB_NAME) < db/schema.sql
