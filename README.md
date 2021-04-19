# dist-updater

This tool is a part of Quickdocs to download extracted dist data from GCS and store it into RDBMS for easy access by the web app.

## Requirement

* Docker Engine

Requires a PostgreSQL server to be accessible from the running machine. The default is localhost:5432.

If runs PostgreSQL with Docker, run the following command:

```
$ docker run --rm -it -p 5432:5432 -v ${PWD}/db/data:/var/lib/postgresql/data -e POSTGRES_DB=quickdocs -e POSTGRES_USER=quickdocs -e POSTGRES_PASSWORD=quickdocs postgres:10.1
```

To initialize the database, run `make db` at this project root.

## Usage

```
# Build a Docker image
$ make build

# Update the DB
$ make VERSION=2021-02-28 run
```
