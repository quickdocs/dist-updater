FROM fukamachi/qlot

WORKDIR /app

COPY qlfile /app
COPY qlfile.lock /app

RUN set -x; \
  qlot install --no-deps

FROM fukamachi/sbcl:2.1.8

WORKDIR /app
COPY --from=0 /app/.qlot /app/.qlot

RUN set -x; \
  mkdir -p "$HOME/.config/common-lisp/source-registry.conf.d/" && \
  echo '(:tree "/app")' >> "$HOME/.config/common-lisp/source-registry.conf.d/app.conf"

ENTRYPOINT ["/app/entrypoint.sh"]
