(defpackage #:dist-updater/command
  (:use #:cl)
  (:import-from #:dist-updater/loader
                #:load-json)
  (:import-from #:dist-updater/external/download-stats
                #:load-download-stats)
  (:import-from #:dist-updater/db
                #:migrate)
  (:export #:main))
(in-package #:dist-updater/command)

(defun setup () (migrate))

(defun help ()
  (format *error-output*
          "~&Usage: dist-updater COMMAND [ARGUMENTS...]

COMMAND:
  update [dist-version]
    Update (or create) dist data with GCS files.

  fetch [external-data]
    Fetch additional data from external resources.

  setup | migrate
    Initialize the database. If the tables already exist, run migrations if any available.

  help
    Show this message.
"))

(defun update (dist-version)
  (unless (and (stringp dist-version)
               (not (uiop:emptyp dist-version)))
    (format *error-output* "~&The dist version is required.~%")
    (uiop:quit -1))

  ;; For now, just download dist JSON and insert rows into DB
  (load-json dist-version))

(defun fetch (external-data)
  (unless (and (stringp external-data)
               (member external-data
                       '("download-stats")
                       :test #'string=))
    (format *error-output* "~&Unknown external resource: ~S~%" external-data)
    (uiop:quit -1))
  (cond
    ((equal external-data "download-stats")
     (load-download-stats))
    ((null external-data)
     (format *error-output* "~&External resource name is required.~%")
     (uiop:quit -1))
    (t
     (format *error-output* "~&Unknown external resource: ~S~%" external-data)
     (uiop:quit -1))))

(defun main ()
  (destructuring-bind ($0 &optional (subcommand "help") &rest args)
      sb-ext:*posix-argv*
    (declare (ignore $0))
    (cond
      ((string= subcommand "update")
       (update (first args)))
      ((string= subcommand "fetch")
       (fetch (first args)))
      ((string= subcommand "setup")
       (setup))
      ((string= subcommand "migrate")
       (migrate))
      (t
       (help)))))
