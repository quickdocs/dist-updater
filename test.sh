#!/bin/bash

set -eux

export DB_HOST=quickdocs-dist-updater-testdb
export DB_PORT=5432
export DB_NAME=quickdocs
export DB_USERNAME=quickdocs
export DB_PASSWORD=quickdocs

sbcl --noinform --non-interactive \
  --eval '(ql:quickload :rove)' \
  --eval '(rove:run :dist-updater-tests)'