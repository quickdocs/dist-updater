#!/bin/bash

set -eux

DB_HOST=quickdocs-dist-updater-testdb
DB_PORT=5432
DB_NAME=quickdocs
DB_USERNAME=quickdocs
DB_PASSWORD=quickdocs

sbcl --noinform --non-interactive \
  --eval '(ql:quickload :rove)' \
  --eval '(rove:run :dist-updater)'
