(defpackage #:dist-updater/external/topics
  (:use #:cl)
  (:import-from #:dist-updater/models
                #:project-topic)
  (:import-from #:dist-updater/db
                #:with-connection)
  (:import-from #:dist-updater/http
                #:fetch-json)
  (:import-from #:mito)
  (:import-from #:cl-dbi)
  (:import-from #:yason)
  (:export #:load-topics))
(in-package #:dist-updater/external/topics)

(defparameter *topics-index-url*
  "https://storage.googleapis.com/quickdocs-resources/cliki/index.json")

(defun topics-latest-info ()
  (fetch-json *topics-index-url*))

(defun get-project-topics (url)
  (fetch-json url))

(defun load-topics ()
  (let* ((info (topics-latest-info))
         (latest-url (gethash "latest_url" info))
         (project-topics (get-project-topics latest-url)))
    (format t "~&Loading topics generated at ~A...~%" (gethash "version" info))
    (with-connection
      (dbi:with-transaction mito:*connection*
        (loop for project-name being the hash-keys of project-topics
              using (hash-value topics)
              do (mito:delete-by-values 'project-topic
                                        :project-name project-name)
                 (dolist (topic topics)
                   (mito:create-dao 'project-topic
                                    :date (gethash "version" info)
                                    :project-name project-name
                                    :topic topic)))))
    (format t "~&Loaded topics of ~D projects.~%"
            (hash-table-count project-topics))))
