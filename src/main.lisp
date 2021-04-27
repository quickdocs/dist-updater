(defpackage #:dist-updater/main
  (:nicknames #:dist-updater)
  (:use #:cl
        #:alexandria
        #:dist-updater/utils/fetch
        #:dist-updater/utils/json-db-class
        #:dist-updater/db-classes)
  (:import-from #:yason)
  (:import-from #:mito)
  (:export #:main))
(in-package #:dist-updater/main)

(defparameter *info-json-url*
  "https://storage.googleapis.com/quickdocs-dist/quicklisp/2021-02-28/info.json")
(defparameter *releases-json-url*
  "https://storage.googleapis.com/quickdocs-dist/quicklisp/2021-02-28/releases.json")

;;;
(defmacro with-transaction* ((&key (rollback t)) &body body)
  `(dbi:with-transaction mito:*connection*
     (let ((mito:*mito-logger-stream* *standard-output*))
       (prog1 (progn ,@body)
         (when ,rollback (dbi:rollback mito:*connection*))))))

;;;
(defun fetch-and-create-release-db (dist)
  (maphash (lambda (name url)
             (format t "~&fetch ~A, ~A~&" name url)
             (when-let ((release-json (yason:parse (fetch url))))
               (setf (gethash "dist" release-json) dist)
               (let ((release (convert-json 'release release-json)))
                 (when-let* ((json-string (fetch (release-readme-url release)))
                             (readme-json (unless (emptyp json-string)
                                            (yason:parse json-string))))
                   (convert-json 'readme readme-json)))))
           (yason:parse (fetch *releases-json-url*))))

(defun create-system-db (release systems-json)
  (let ((systems '()))
    (maphash (lambda (system-name system-json)
               (declare (ignore system-name))
               (setf (gethash "release" system-json) release)
               (push (convert-json 'system system-json)
                     systems))
             systems-json)
    systems))

(defun create-all-systems-db ()
  (loop :with releases := (mito:select-dao 'release)
        :with all-nums := (length releases)
        :for release :in releases
        :for progress :from 0
        :do (format t "~&~A ~50T ~D/~D~&" (release-project-name release) progress all-nums)
        :do (let* ((url (release-systems-metadata-url release))
                   (json (yason:parse (fetch url)))
                   (systems (create-system-db release json)))
              (validate json systems))))

(defun find-or-create-dist (dist-version)
  (or (mito:find-dao 'dist
                     :name "quicklisp"
                     :version dist-version)
      (mito:create-dao 'dist
                       :name "quicklisp"
                       :version dist-version)))

(defun main (dist-version &rest connect-args)
  (unless (and (stringp dist-version)
               (not (uiop:emptyp dist-version)))
    (format *error-output* "~&The dist version is required.~%")
    (uiop:quit -1))

  (let ((mito:*connection* (apply #'dbi:connect :postgres connect-args)))
    (unwind-protect
        (let ((dist (find-or-create-dist dist-version)))
          (fetch-and-create-release-db dist)
          (create-all-systems-db))
      (dbi:disconnect mito:*connection*))))

;;; test
(defmacro assert* (form)
  `(with-simple-restart (continue "ignore") (assert ,form)))

(defun validate (json systems)
  (dolist (system systems)
    (let ((json (gethash (system-name system) json))
          (system (mito:find-dao 'system :id (mito:object-id system))))
      (assert* (equal (gethash "name" json)
                      (system-name system)))
      (assert* (equal (gethash "system_file_name" json)
                      (system-system-file-name system)))
      (assert* (equal (gethash "required_systems" json)
                      (coerce (system-required-systems system) 'list)))
      (let ((json (gethash "metadata" json))
            (metadata (system-metadata system)))
        (unless (and (null json) (null metadata))
          (assert* (equal (gethash "name" json)
                          (system-metadata-name metadata)))
          (assert* (equal (gethash "long_name" json)
                          (system-metadata-long-name metadata)))
          (assert* (equal (gethash "version" json)
                          (system-metadata-version metadata)))
          (assert* (equal (let ((val (gethash "description" json)))
                            (if (consp val)
                                (first val)
                                val))
                          (system-metadata-description metadata)))
          (assert* (equal (gethash "long_description" json)
                          (system-metadata-long-description metadata)))
          (assert* (equalp (coerce (ensure-list (gethash "author" json)) 'vector)
                           (coerce (system-metadata-author metadata) 'vector)))
          (assert* (equalp (dist-updater/db-classes::normalize-array (gethash "maintainer" json))
                           (coerce (system-metadata-maintainer metadata) 'vector)))
          (assert* (equal (gethash "mailto" json)
                          (system-metadata-mailto metadata)))
          (assert* (equal (gethash "license" json)
                          (system-metadata-license metadata)))
          (assert* (equal (gethash "homepage" json)
                          (system-metadata-homepage metadata)))
          (assert* (equal (gethash "bug_tracker" json)
                          (system-metadata-bug-tracker metadata)))
          (assert* (and (or (null (first (ensure-list (gethash "source_control" json))))
                            (equal (first (ensure-list (gethash "source_control" json)))
                                   (first (coerce (system-metadata-source-control metadata) 'list))))
                        (equal (second (ensure-list (gethash "source_control" json)))
                               (second (coerce (system-metadata-source-control metadata) 'list)))))
          (flet ((validate-depends-on (json-list depends-on-list)
                   (assert* (= (length json-list)
                               (length depends-on-list)))
                   (set-equal (mapcar (lambda (json) (gethash "name" json)) json-list)
                              (mapcar #'abstract-metadata-depends-on-name depends-on-list)
                              :test #'equal)
                   (set-equal (mapcar (lambda (json) (gethash "version" json)) json-list)
                              (mapcar #'abstract-metadata-depends-on-version depends-on-list)
                              :test #'equal)
                   (set-equal (mapcar (lambda (json) (gethash "feature" json)) json-list)
                              (mapcar #'abstract-metadata-depends-on-feature depends-on-list)
                              :test #'equal)))
            (validate-depends-on (gethash "defsystem_depends_on" json)
                                 (system-metadata-defsystem-depends-on metadata))
            (validate-depends-on (gethash "depends_on" json)
                                 (system-metadata-depends-on metadata))
            (validate-depends-on (gethash "weakly_depends_on" json)
                                 (system-metadata-weakly-depends-on metadata))))))))
