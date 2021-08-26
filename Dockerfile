FROM fukamachi/sbcl

WORKDIR /app

ENTRYPOINT ["/app/entrypoint.sh"]
