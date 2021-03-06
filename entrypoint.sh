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

export QUICKLISP_HOME=.qlot/

# Use the binary if exists
if [ -f "./dist-updater" ]; then
  exec ./dist-updater "$subcommand" "$@"
else
  exec ros ./roswell/dist-updater.ros \
    "$subcommand" "$@"
fi
