(defpackage #:dist-updater/command
  (:use #:cl)
  (:import-from #:dist-updater/loader
                #:load-json
                #:latest-dist-version
                #:not-supported-dist-version)
  (:import-from #:dist-updater/external/download-stats
                #:load-download-stats)
  (:import-from #:dist-updater/external/topics
                #:load-topics)
  (:import-from #:dist-updater/db
                #:migrate)
  (:export #:main))
(in-package #:dist-updater/command)

(defun setup () (migrate))

(defun help ()
  (format *error-output*
          "~&Usage: dist-updater COMMAND [ARGUMENTS...]

COMMAND:
  update [dist-version] [--force]
    Update (or create) dist data with GCS files.
    If dist-version is omitted, load the latest version by default.

  fetch [external-data]
    Fetch additional data from external resources.

  setup | migrate
    Initialize the database. If the tables already exist, run migrations if any available.

  help
    Show this message.
"))

(defun update (dist-version &key force)
  (let ((dist-version
          (if (and (stringp dist-version)
                   (not (uiop:emptyp dist-version)))
              dist-version
              (latest-dist-version))))

    (handler-case
        (load-json dist-version :force force)
      (not-supported-dist-version (e)
        (format *error-output* "~&~A~%" e)
        (uiop:quit -1)))))

(defvar *external-resources*
  '(("download-stats" . load-download-stats)
    ("topics" . load-topics)))

(defun fetch (external-data)
  (unless external-data
    (format *error-output* "~&External resource name is required.~%")
    (uiop:quit -1))

  (let ((action (cdr (assoc external-data *external-resources*
                            :test #'equal))))
    (unless action
      (format *error-output* "~&Unknown external resource: ~S~%" external-data)
      (uiop:quit -1))

    (funcall action)))

(defmacro case-equal (keyform &body cases)
  (let ((g-keyform (gensym "KEYFORM")))
    `(let ((,g-keyform ,keyform))
       (cond
         ,@(loop for (case . body) in cases
                 if (eq case 'otherwise)
                   collect `(t ,@body)
                 else
                   collect `((find ,g-keyform ',(if (listp case)
                                                    case
                                                    (list case))
                                   :test #'equal)
                             ,@body))))))

(defun parse-args (args)
  (let ((force nil)
        (new-args '()))
    (loop for option = (pop args)
          while option
          do (case-equal option
               ("--force" (setf force t))
               (otherwise
                 (push option new-args)))
          finally
          (return (values (reverse new-args)
                          (list :force force))))))

(defun main ()
  (destructuring-bind ($0 &optional (subcommand "help") &rest args)
      sb-ext:*posix-argv*
    (declare (ignore $0))
    (multiple-value-bind (args options)
        (parse-args args)
      (cond
        ((string= subcommand "update")
         (apply #'update (first args) options)
         (mapc #'fetch (mapcar #'first *external-resources*)))
        ((string= subcommand "fetch")
         (fetch (first args)))
        ((string= subcommand "setup")
         (setup))
        ((string= subcommand "migrate")
         (migrate))
        (t
         (help))))))
