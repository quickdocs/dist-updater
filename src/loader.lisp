(defpackage #:dist-updater/loader
  (:use #:cl
        #:alexandria)
  (:import-from #:dist-updater/models
                #:dist
                #:dist-name
                #:dist-version
                #:dist-provided-releases-count
                #:dist-release
                #:release
                #:release-dist-name
                #:release-dist-version
                #:release-name
                #:release-archive-url
                #:release-archive-size
                #:release-archive-content-sha1
                #:release-prefix
                #:release-systems-metadata-url
                #:release-readme-url
                #:release-upstream-url
                #:readme-file
                #:system
                #:system-dependency)
  (:import-from #:dist-updater/db
                #:with-connection)
  (:import-from #:cl-dbi)
  (:import-from #:mito)
  (:import-from #:sxql
                #:where
                #:left-join
                #:limit)
  (:import-from #:dexador)
  (:import-from #:yason)
  (:export #:latest-dist-version
           #:load-json))
(in-package #:dist-updater/loader)

(define-condition not-supported-dist-version (error)
  ((dist-name :initarg :dist-name)
   (dist-version :initarg :dist-version))
  (:report (lambda (condition stream)
             (with-slots (dist-name dist-version) condition
               (format stream "Not supported dist version: ~A (version=~A)"
                       dist-name dist-version)))))

(defun fetch-json (url)
  (yason:parse (dex:get url)))

(defun latest-dist-version ()
  (let ((dist-info (fetch-json "http://storage.googleapis.com/quickdocs-dist/quicklisp/info.json")))
    (gethash "latest_version" dist-info)))

(defun delete-release (release)
  (dolist (system (mito:select-dao 'system
                    (where (:= :release release))))
    (mapc #'mito:delete-dao
          (mito:select-dao 'system-dependency
            (where (:= :system system))))
    (mito:delete-dao system))
  (mito:delete-by-values 'readme-file :release release))

(defun delete-dist (dist)
  (dolist (release (mito:select-dao 'release
                     (left-join :dist_release :on (:= :dist_release.release_id :release.id))
                     (where (:= :dist_release.dist_id (mito:object-id dist)))))
    (delete-release release)
    (unless (mito:select-dao 'dist-release
              (where (:and (:= :release_id (mito:object-id release))
                           (:!= :dist_id (mito:object-id dist))))
              (limit 1))
      (mito:delete-dao release)))
  (mito:delete-by-values 'dist-release :dist dist)
  (mito:delete-dao dist))

(defgeneric create-from-hash (class data &key)
  (:method :before (class data &key)
    (check-type data hash-table)))

(defmethod create-from-hash ((class (eql 'dist)) data &key force)
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
                                 :progress progress
                                 :force force)))))

(defmethod create-from-hash ((class (eql 'release)) data &key dist progress force)
  (format t "~&[~D / ~D] ~A"
          progress
          (dist-provided-releases-count dist)
          (gethash "project_name" data))
  (force-output)
  (let* ((name (gethash "project_name" data))
         (archive-url (gethash "archive_url" data))
         (systems-metadata-url (gethash "systems_metadata_url" data))
         (readme-url (gethash "readme_url" data))
         (dist-version (dist-version dist))
         (release
           (mito:find-dao class
                          :dist-name (dist-name dist)
                          :dist-version dist-version
                          :name name))
         (label (and (null release)
                     (if (mito:find-dao class
                                        :dist-name (dist-name dist)
                                        :name name)
                         "updated"
                         "new"))))
    (format t "~@[ (~A)~]~%" label)

    (when (or (null release)
              force)
      (cond
        (release
         (setf (release-dist-name release) (dist-name dist)
               (release-dist-version release) dist-version
               (release-name release) name
               (release-archive-url release) archive-url
               (release-archive-size release) (gethash "archive_size" data)
               (release-archive-content-sha1 release) (gethash "archive_content_sha1" data)
               (release-prefix release) (gethash "prefix" data)
               (release-systems-metadata-url release) systems-metadata-url
               (release-readme-url release) readme-url
               (release-upstream-url release) (gethash "upstream_url" data))
         (mito:save-dao release)
         (when force
           (delete-release release)))
        (t
         (setf release
               (mito:create-dao class
                                :dist-name (dist-name dist)
                                :dist-version dist-version
                                :name name
                                :archive-url archive-url
                                :archive-size (gethash "archive_size" data)
                                :archive-content-sha1 (gethash "archive_content_sha1" data)
                                :prefix (gethash "prefix" data)
                                :systems-metadata-url systems-metadata-url
                                :readme-url readme-url
                                :upstream-url (gethash "upstream_url" data)))))

      (let ((systems-metadata (fetch-json systems-metadata-url)))
        (when systems-metadata
          (cond
            ((gethash name systems-metadata)
             (setf (gethash "is_primary" (gethash name systems-metadata)) t))
            ((and (starts-with-subseq "cl-" name)
                  (gethash (subseq name 3) systems-metadata))
             (setf (gethash "is_primary" (gethash (subseq name 3) systems-metadata)) t))
            (t
             (loop for system-name being the hash-keys of systems-metadata
                   using (hash-value metadata)
                   if (and (starts-with-subseq "cl-" system-name)
                           (equal (subseq system-name 3) name))
                   do (setf (gethash "is_primary" metadata) t)
                   (return))))
          (maphash (lambda (name metadata)
                     (declare (ignore name))
                     (create-from-hash 'system metadata
                                       :release release))
                   systems-metadata)))
      (let ((readme-data (fetch-json readme-url)))
        (dolist (readme-file (gethash "readme_files" readme-data))
          (create-from-hash 'readme-file readme-file
                            :release release))))
    (unless (mito:find-dao 'dist-release :dist dist :release release)
      (mito:create-dao 'dist-release
                       :dist dist
                       :release release))))

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
                             :is-primary (gethash "is_primary" data)
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
                   :feature (and (gethash "feature" data)
                                 (prin1-to-string (gethash "feature" data)))))

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

(defun create-dist (dist-version &key (name "quicklisp") force)
  (let ((data (handler-case (fetch-json (dist-info-url name dist-version))
                (dex:http-request-not-found ()
                  (error 'not-supported-dist-version
                         :dist-name name
                         :dist-version dist-version)))))
    (create-from-hash 'dist data :force force)))

(defun load-json (dist-version &key force)
  (check-type dist-version string)

  (with-connection
    (dbi:with-transaction mito:*connection*
      (let ((dist (find-dist dist-version)))
        (when dist
          (if force
              (delete-dist dist)
              (progn
                (format t "~&Already have a dist version '~A'. Skipped.~%" dist-version)
                (format t "~&Use --force option to overwrite it.~%")
                (return-from load-json)))))
      (create-dist dist-version :force force)
      (format t "~&A dist version '~A' is loaded.~%" dist-version))))
