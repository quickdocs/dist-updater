(defpackage #:dist-updater/db
  (:use #:cl)
  (:import-from #:cl-dbi
                #:connect
                #:disconnect)
  (:import-from #:mito
                #:*connection*
                #:migrate)
  (:export #:migrate
           #:with-connection))
(in-package #:dist-updater/db)

(defun connection-arguments ()
  (list :host (or (uiop:getenv "DB_HOST") "localhost")
        :port (parse-integer (or (uiop:getenv "DB_PORT") "5432"))
        :database-name (or (uiop:getenv "DB_NAME") "quickdocs")
        :username (or (uiop:getenv "DB_USERNAME") "quickdocs")
        :password (or (uiop:getenv "DB_PASSWORD") "quickdocs")))

(defmacro with-connection (&body body)
  `(let ((mito:*connection* (apply #'dbi:connect :postgres
                                   (connection-arguments))))
    (unwind-protect
        (progn ,@body)
      (dbi:disconnect mito:*connection*))))

(defun migrate ()
  (with-connection
    (mito:migrate #P"db/")))
