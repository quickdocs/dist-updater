(defpackage #:dist-updater/external/topics
  (:use #:cl)
  (:import-from #:dist-updater/models
                #:project-topic)
  (:import-from #:dist-updater/db
                #:with-connection)
  (:import-from #:mito)
  (:import-from #:cl-dbi)
  (:import-from #:dexador)
  (:import-from #:yason)
  (:export #:load-topics))
(in-package #:dist-updater/external/topics)

(defparameter *topics-index-url*
  "https://storage.googleapis.com/quickdocs-resources/cliki/index.json")

(defun topics-latest-info ()
  (let ((body (dex:get *topics-index-url*)))
    (yason:parse body)))

(defun get-project-topics (url)
  (let ((body (dex:get url)))
    (yason:parse body)))

(defun load-topics ()
  (let* ((info (topics-latest-info))
         (latest-url (gethash "latest_url" info))
         (project-topics (get-project-topics latest-url)))
    (format t "~&Loading topics generated at ~A...~%" (gethash "version" info))
    (with-connection
      (dbi:with-transaction mito:*connection*
        (loop for project-name being the hash-keys of project-topics
              using (hash-value topics)
              do (dolist (topic topics)
                   (mito:create-dao 'project-topic
                                    :date (gethash "version" info)
                                    :project-name project-name
                                    :topic topic)))))
    (format t "~&Loaded topics of ~D projects.~%"
            (hash-table-count project-topics))))
