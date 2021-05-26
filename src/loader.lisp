(defpackage #:dist-updater/loader
  (:use #:cl
        #:alexandria)
  (:import-from #:dist-updater/models
                #:dist
                #:dist-name
                #:dist-version
                #:dist-provided-releases-count
                #:release
                #:readme-file
                #:system
                #:system-dependency)
  (:import-from #:dist-updater/db
                #:with-connection)
  (:import-from #:cl-dbi)
  (:import-from #:mito)
  (:import-from #:sxql
                #:where)
  (:import-from #:dexador)
  (:import-from #:yason)
  (:export #:latest-dist-version
           #:load-json))
(in-package #:dist-updater/loader)

(defun fetch-json (url)
  (yason:parse (dex:get url)))

(defun latest-dist-version ()
  (let ((dist-info (fetch-json "http://storage.googleapis.com/quickdocs-dist/quicklisp/info.json")))
    (gethash "latest_version" dist-info)))

(defgeneric create-from-hash (class data &key)
  (:method :before (class data &key)
    (check-type data hash-table)))

(defmethod create-from-hash ((class (eql 'dist)) data &key)
  (let ((provided-releases-url (gethash "provided_releases_url" data)))
    (let ((dist
            (mito:create-dao class
                             :name (gethash "name" data)
                             :version (gethash "version" data)
                             :system-index-url (gethash "system_index_url" data)
                             :release-index-url (gethash "release_index_url" data)
                             :archive-base-url (gethash "archive_base_url" data)
                             :distinfo-subscription-url (gethash "distinfo_subscription_url" data)
                             :canonical-distinfo-url (gethash "canonical_distinfo_url" data)
                             :provided-releases-count (gethash "provided_releases_count" data)
                             :provided-releases-url provided-releases-url
                             :extract-errors-url (gethash "extract_errors_url" data)))
          (releases (fetch-json provided-releases-url)))
      (loop for name being the hash-keys of releases
            using (hash-value release-url)
            for progress from 1
            for release = (fetch-json release-url)
            do (create-from-hash 'release release
                                 :dist dist
                                 :progress progress)))))

(defmethod create-from-hash ((class (eql 'release)) data &key dist progress)
  (format t "~&[~D / ~D] ~A~%"
          progress
          (dist-provided-releases-count dist)
          (gethash "project_name" data))
  (let* ((systems-metadata-url (gethash "systems_metadata_url" data))
         (readme-url (gethash "readme_url" data))
         (release (mito:create-dao class
                                   :dist dist
                                   :name (gethash "project_name" data)
                                   :archive-url (gethash "archive_url" data)
                                   :archive-size (gethash "archive_size" data)
                                   :archive-content-sha1 (gethash "archive_content_sha1" data)
                                   :prefix (gethash "prefix" data)
                                   :systems-metadata-url systems-metadata-url
                                   :readme-url readme-url
                                   :upstream-url (gethash "upstream_url" data))))
    (let ((systems-metadata (fetch-json systems-metadata-url)))
      (maphash (lambda (name metadata)
                 (declare (ignore name))
                 (create-from-hash 'system metadata
                                   :release release))
               systems-metadata))
    (let ((readme-data (fetch-json readme-url)))
      (dolist (readme-file (gethash "readme_files" readme-data))
        (create-from-hash 'readme-file readme-file
                          :release release)))))

(defmethod create-from-hash ((class (eql 'readme-file)) data &key release)
  (mito:create-dao class
                   :release release
                   :filename (gethash "filename" data)
                   :content (gethash "content" data)))

(defmethod create-from-hash ((class (eql 'system)) data &key release)
  (let ((metadata (or (gethash "metadata" data)
                      (make-hash-table))))
    (let ((system
            (mito:create-dao class
                             :release release
                             :name (gethash "name" data)
                             :filename (gethash "system_file_name" data)
                             :long-name (gethash "long_name" metadata)
                             :version (gethash "version" metadata)
                             :description (gethash "description" metadata)
                             :long-description (gethash "long_description" metadata)
                             :authors (coerce (gethash "authors" metadata) 'vector)
                             :maintainers (coerce (gethash "maintainers" metadata) 'vector)
                             :mailto (gethash "mailto" metadata)
                             :license (gethash "license" metadata)
                             :homepage (gethash "homepage" metadata)
                             :bug-tracker (gethash "bug_tracker" metadata)
                             :source-control-url (second (gethash "source_control" metadata)))))
      (loop for (type dependencies) on (list "defsystem" (gethash "defsystem_depends_on" metadata)
                                             "normal"    (gethash "depends_on" metadata)
                                             "weakly"    (gethash "weakly_depends_on" metadata)) by #'cddr
            for unique-dependencies = (remove-duplicates dependencies
                                                         :test #'equal
                                                         :key (lambda (v) (gethash "name" v))
                                                         :from-end t)
            do (dolist (dependency unique-dependencies)
                 (create-from-hash 'system-dependency dependency
                                   :system system
                                   :type type))))))

(defmethod create-from-hash ((class (eql 'system-dependency)) data &key system type)
  (mito:create-dao class
                   :system system
                   :type type
                   :name (gethash "name" data)
                   :version (gethash "version" data)
                   :feature (prin1-to-string (gethash "feature" data))))

(defparameter *dist-info-url-template*
  "https://storage.googleapis.com/quickdocs-dist/~A/~A/info.json")
(defparameter *releases-json-url-template*
  "https://storage.googleapis.com/quickdocs-dist/~A/~A/releases.json")

(defun dist-info-url (name dist-version)
  (format nil *dist-info-url-template* name dist-version))

(defun releases-json-url (dist)
  (format nil *releases-json-url-template*
          (dist-name dist)
          (dist-version dist)))

(defun find-dist (dist-version &key (name "quicklisp"))
  (mito:find-dao 'dist
                 :name name
                 :version dist-version))

(defun create-dist (dist-version &key (name "quicklisp"))
  (let ((data (fetch-json (dist-info-url name dist-version))))
    (create-from-hash 'dist data)))

(defun delete-dist (dist)
  (dolist (release (mito:select-dao 'release
                     (where (:= :dist dist))))
    (dolist (system (mito:select-dao 'system
                      (where (:= :release release))))
      (mapc #'mito:delete-dao
            (mito:select-dao 'system-dependency
              (where (:= :system system))))
      (mito:delete-dao system))
    (mito:delete-dao release))
  (mito:delete-dao dist))

(defun load-json (dist-version)
  (check-type dist-version string)

  (with-connection
    (dbi:with-transaction mito:*connection*
      (let ((dist (find-dist dist-version)))
        (when dist
          (delete-dist dist)))
      (create-dist dist-version))))