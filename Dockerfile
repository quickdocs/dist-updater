FROM fukamachi/sbcl

WORKDIR /app

RUN set -x; \
  ros -S . -s dist-updater/command

ENTRYPOINT ["/app/entrypoint.sh"]
