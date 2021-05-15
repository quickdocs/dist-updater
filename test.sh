#!/bin/bash

set -eux

sbcl --noinform --non-interactive \
  --eval '(ql:quickload :rove)' \
  --eval '(rove:run :dist-updater)'
