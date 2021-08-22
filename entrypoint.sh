#!/bin/bash

case $1 in
  update|fetch|setup|migrate|help)
    subcommand=$1
    shift
    ;;
  *)
    subcommand=help
    ;;
esac

# Use the binary if exists
if [ -f "./dist-updater" ]; then
  exec ./dist-updater "$subcommand" "$@"
else
  exec sbcl --noinform --non-interactive \
    --eval '(progn (format *error-output* "~&Loading...~%") (ql:quickload :dist-updater/command :silent t))' \
    --eval "(dist-updater/command:main)" \
    "$subcommand" "$@"
fi
