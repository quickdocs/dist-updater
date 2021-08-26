FROM fukamachi/sbcl

WORKDIR /app

RUN set -x; \
  mkdir -p "$HOME/.config/common-lisp/source-registry.conf.d/" && \
  echo '(:tree "/app")' >> "$HOME/.config/common-lisp/source-registry.conf.d/app.conf"

ENTRYPOINT ["/app/entrypoint.sh"]
