all: build

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
	docker build -t quickdocs/dist-updater-dev -f Dockerfile .

.PHONY: build_test
build_test: build
	docker build -t quickdocs/dist-updater-test -f Dockerfile.test .

.PHONY: update
update:
	docker run --rm -it -v ${PWD}:/app --net=$(NETWORK) \
		-e DB_HOST=$(DB_HOST) -e DB_PORT=$(DB_PORT) \
		-e DB_NAME=$(DB_NAME) -e DB_USERNAME=$(DB_USERNAME) -e DB_PASSWORD=$(DB_PASSWORD) \
		quickdocs/dist-updater-dev update $(version)

.PHONY: fetch
fetch:
	docker run --rm -it -v ${PWD}:/app --net=$(NETWORK) \
		-e DB_HOST=$(DB_HOST) -e DB_PORT=$(DB_PORT) \
		-e DB_NAME=$(DB_NAME) -e DB_USERNAME=$(DB_USERNAME) -e DB_PASSWORD=$(DB_PASSWORD) \
		quickdocs/dist-updater-dev fetch $(resource)

.PHONY: generate-migrations
generate-migrations:
	docker run --rm -it -v ${PWD}:/app --net=$(NETWORK) --entrypoint sbcl quickdocs/dist-updater-dev \
		--noinform --non-interactive \
		--eval '(ql:quickload :dist-updater)' \
		--eval '(mito:connect-toplevel :postgres :database-name "$(DB_NAME)" :host "$(DB_HOST)" :port $(DB_PORT) :username "$(DB_USERNAME)" :password "$(DB_PASSWORD)")' \
		--eval '(mito:generate-migrations #P"db/")'

.PHONY: migrate
migrate:
	docker run --rm -i -v ${PWD}:/app --net=$(NETWORK) \
		-e DB_HOST=$(DB_HOST) -e DB_PORT=$(DB_PORT) \
		-e DB_NAME=$(DB_NAME) -e DB_USERNAME=$(DB_USERNAME) -e DB_PASSWORD=$(DB_PASSWORD) \
		quickdocs/dist-updater-dev migrate

.PHONY: test
test: test_network testdb
	docker run --rm -i --network quickdocs_dist_updater_test -v ${PWD}:/app quickdocs/dist-updater-test
	$(test_cleanup)

.PHONY: testdb
testdb:
	$(test_cleanup)
	docker run -d --name quickdocs-dist-updater-testdb \
		--network quickdocs_dist_updater_test \
		-e POSTGRES_DB=quickdocs -e POSTGRES_USER=quickdocs -e POSTGRES_PASSWORD=quickdocs \
		postgres:10.1
	$(MAKE) migrate NETWORK=quickdocs_dist_updater_test DB_HOST=quickdocs-dist-updater-testdb

.PHONY: test_network
test_network:
	docker network inspect quickdocs_dist_updater_test >/dev/null 2>&1 || \
		docker network create --driver bridge quickdocs_dist_updater_test 2>/dev/null

test_cleanup = docker inspect --type=container --format '{{json .Id}}' quickdocs-dist-updater-testdb >/dev/null 2>&1 && \
		(docker kill quickdocs-dist-updater-testdb >/dev/null; docker rm -v quickdocs-dist-updater-testdb >/dev/null) || true
