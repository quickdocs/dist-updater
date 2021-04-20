# dist-updater

This tool is a part of Quickdocs to download extracted dist data from GCS and store it into RDBMS for easy access by the web app.

## Requirement

* Docker Engine
* jq

Requires a PostgreSQL server to be accessible from the running machine. The default is localhost:5432.

If runs PostgreSQL with Docker, run the following command:

```
$ docker run -d -p 5432:5432 -v ${PWD}/db/data:/var/lib/postgresql/data -e POSTGRES_DB=quickdocs -e POSTGRES_USER=quickdocs -e POSTGRES_PASSWORD=quickdocs postgres:10.1
$ make db DB_PORT=5432
```

To initialize the database, run `make db` at this project root.

## Usage

```
# Build a Docker image
$ make build

# Update the DB
$ make run version=2021-02-28 DB_PORT=5432
```
