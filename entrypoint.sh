#!/bin/bash

options=`getopt -u -o '' --longoptions 'host:,port:,dbname:,username:,password:' -- "$@"`

set -- $options
for option in "$@"
do
  case $option in
    --host)
      host=$2
      shift 2
      ;;
    --port)
      port=$2
      shift 2
      ;;
    --dbname)
      dbname=$2
      shift 2
      ;;
    --username)
      username=$2
      shift 2
      ;;
    --password)
      password=$2
      shift 2
      ;;
    --)
      shift
      break
      ;;
  esac
done

version=$1

if [ "$version" = "" ]; then
  echo "The dist version is required."
  exit -1
fi

sbcl --noinform --non-interactive \
  --eval '(progn (format *error-output* "~&Loading...~%") (ql:quickload :dist-updater :silent t))' \
  --eval "(dist-updater:main \"$version\" :host \"$host\" :port $port :database-name \"$dbname\" :username \"$username\" :password \"$password\")"
