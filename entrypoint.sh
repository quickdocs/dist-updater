#!/bin/bash

case $1 in
  update|setup|migrate|help)
    subcommand=$1
    shift
    ;;
  *)
    subcommand=help
    ;;
esac

# Use the binary if exists
if [ -f "./dist-updater" ]; then
  ./dist-updater "$subcommand" "$@"
else
  sbcl --noinform --non-interactive \
    --eval '(progn (format *error-output* "~&Loading...~%") (ql:quickload :dist-updater :silent t))' \
    --eval "(let ((sb-ext:*posix-argv* (list \"\" \"$subcommand\" \"$@\"))) (dist-updater:main))"
fi
