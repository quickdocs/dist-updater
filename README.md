# dist-updater

This tool is a part of Quickdocs to download extracted dist data from GCS and store it into RDBMS for easy access by the web app.

## Requirement

* Docker Engine

Requires a PostgreSQL server to be accessible from the running machine. The default is localhost:5432.

If runs PostgreSQL with Docker, run the following command:

```
$ docker run -d -p 5432:5432 -v ${PWD}/db/data:/var/lib/postgresql/data -e POSTGRES_DB=quickdocs -e POSTGRES_USER=quickdocs -e POSTGRES_PASSWORD=quickdocs postgres:10.1
```

## Usage

```
# Initialize the database. (requires only the first time)
$ docker run --rm -it ghcr.io/quickdocs/dist-updater setup

# Run update
$ docker run --rm -it ghcr.io/quickdocs/dist-updater update 2021-02-28
# Access to DB on the other host/port (ex. 10.0.9.14:15432)
$ docker run --rm -it -e DB_HOST=10.0.9.14 -e DB_PORT=15432 ghcr.io/quickdocs/dist-updater update 2021-02-28
```
