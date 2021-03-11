(defpackage :dist-updater/main
  (:use :cl
        :alexandria
        :dist-updater/db
        :dist-updater/json-db-class
        :dist-updater/fetch)
  (:import-from :yason)
  (:import-from :mito))
(in-package :dist-updater/main)

#+(or)
(defmethod print-object ((object hash-table) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (hash-table-alist object) stream)))

;;; release
(define-json-db-class release ()
  ((project-name :col-type :text)
   (archive-url :col-type :text)
   (archive-size :col-type :integer)
   (archive-content-sha1 :col-type (:varchar 40))
   (prefix :col-type :text)
   (systems-metadata-url :col-type :text)
   (systems :relational-type release-system :type list)))

(define-json-db-class release-system ()
  ((release :col-type release)
   (name :col-type :text)
   (system-file-name :col-type :text)
   (required-systems :col-type :text[])))

;;; system.metadata
(define-json-db-class system-metadata ()
  ((name :col-type (or :null :text))
   (long-name :col-type (or :null :text))
   (version :col-type (or :null :text))
   (description :col-type (or :null :text))
   (long-description :col-type (or :null :text))
   (author :col-type :text[])
   (maintainer :col-type :text[])
   (mailto :col-type (or :null :text))
   (license :col-type (or :null :text))
   (homepage :col-type (or :null :text))
   (bug-tracker :col-type (or :null :text))
   (source-control :col-type (or :null :text[]))
   (defsystem-depends-on :relational-type metadata-defsystem-depends-on :type list)
   (depends-on :relational-type metadata-depends-on :type list)
   (weakly-depends-on :relational-type metadata-weakly-depends-on :type list)))

(defun normalize-array (value)
  (flet ((ensure-seq (x)
           (typecase x
             (list (coerce x 'vector))
             (string (vector x))
             (vector x))))
    (ensure-seq (if (and (or (consp value) (vectorp value))
                         (length= value 2)
                         (equal "quote" (elt value 0)))
                    (elt value 1)
                    value))))

(defmethod convert-json-aux :around ((dao system-metadata))
  (setf (system-metadata-author dao)
        (normalize-array (system-metadata-author dao)))
  (setf (system-metadata-maintainer dao)
        (normalize-array (system-metadata-maintainer dao)))
  (setf (system-metadata-source-control dao)
        (normalize-array (system-metadata-source-control dao)))
  (setf (system-metadata-description dao)
        (if (consp (system-metadata-description dao))
            (first (system-metadata-description dao))
            (system-metadata-description dao)))
  (call-next-method dao))

(define-json-db-class abstract-metadata-depends-on ()
  ((system-metadata :col-type system-metadata)
   (name :col-type :text)
   (version :col-type (or :null :text))
   (feature :col-type (or :null :text)) ; s-expression to string
   )
  (:abstract t))

(defmethod convert-json-aux :around ((dao abstract-metadata-depends-on))
  (with-slots (feature) dao
    (when feature
      (setf feature
            (prin1-to-string feature))))
  (call-next-method dao))

(defmethod abstract-metadata-depends-on-feature :around ((dao abstract-metadata-depends-on))
  (let ((feature (call-next-method)))
    (if (emptyp feature)
        nil
        (read-from-string feature))))

(define-json-db-class metadata-defsystem-depends-on (abstract-metadata-depends-on) ())
(define-json-db-class metadata-depends-on (abstract-metadata-depends-on) ())
(define-json-db-class metadata-weakly-depends-on (abstract-metadata-depends-on) ())

;;; system
(define-json-db-class system ()
  ((name :col-type :text)
   (system-file-name :col-type :text)
   (required-systems :col-type (or :null :text[]))
   (metadata :col-type (or :null system-metadata)
             :relational-type system-metadata)))

;;;
(defvar *release-table* (make-hash-table))

(defun download-all-releases ()
  (let ((release-table (make-hash-table)))
    (maphash (lambda (name url)
               (print (cons name url))
               (let ((yason (yason:parse (fetch url))))
                 (setf (gethash name release-table) yason)))
             (fetch-releases))
    (setf *release-table* release-table)))

;; for debug
(defvar $release-name)
(defvar $release-yason)
(defvar $system-name)
(defvar $system-json)

(defun fetch-and-create-release-db ()
  (maphash (lambda (name url)
             (format t "~&fetch ~A, ~A~&" name url)
             (let ((json (yason:parse (fetch url))))
               (convert-json 'release json)))
           (fetch-releases)))

(defun create-system-db (systems-json)
  (let ((systems '()))
    (maphash (lambda (system-name system-json)
               (setf $system-name system-name)
               (setf $system-json system-json)
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
                   (systems (create-system-db json)))
              (validate json systems))))

(defun main ()
  (ensure-connection)
  (fetch-and-create-release-db)
  (create-all-systems-db)
  )

;;;
(defun delete-systems ()
  (mito:delete-by-values 'system)
  (mito:delete-by-values 'system-metadata)
  (mito:delete-by-values 'abstract-metadata-depends-on)
  (mito:delete-by-values 'metadata-defsystem-depends-on)
  (mito:delete-by-values 'metadata-depends-on)
  (mito:delete-by-values 'metadata-weakly-depends-on))

(defun validate (json systems)
  (defparameter $json json)
  (defparameter $systems systems)
  (dolist (system systems)
    (let ((json (gethash (system-name system) json))
          (system (mito:find-dao 'system :id (mito:object-id system))))
      (defparameter $system system)
      (assert (equal (gethash "name" json)
                     (system-name system)))
      (assert (equal (gethash "system_file_name" json)
                     (system-system-file-name system)))
      (assert (equal (gethash "required_systems" json)
                     (coerce (system-required-systems system) 'list)))
      (let ((json (gethash "metadata" json))
            (metadata (system-metadata system)))
        (unless (and (null json) (null metadata))
          (assert (equal (gethash "name" json)
                         (system-metadata-name metadata)))
          (assert (equal (gethash "long_name" json)
                         (system-metadata-long-name metadata)))
          (assert (equal (gethash "version" json)
                         (system-metadata-version metadata)))
          (assert (equal (let ((val (gethash "description" json)))
                           (if (consp val)
                               (first val)
                               val))
                         (system-metadata-description metadata)))
          (assert (equal (gethash "long_description" json)
                         (system-metadata-long-description metadata)))
          (assert (equalp (coerce (ensure-list (gethash "author" json)) 'vector)
                          (coerce (system-metadata-author metadata) 'vector)))
          (assert (equalp (normalize-array (gethash "maintainer" json))
                          (coerce (system-metadata-maintainer metadata) 'vector)))
          (assert (equal (gethash "mailto" json)
                         (system-metadata-mailto metadata)))
          (assert (equal (gethash "license" json)
                         (system-metadata-license metadata)))
          (assert (equal (gethash "homepage" json)
                         (system-metadata-homepage metadata)))
          (assert (equal (gethash "bug_tracker" json)
                         (system-metadata-bug-tracker metadata)))
          (assert (equalp (coerce (ensure-list (gethash "source_control" json)) 'vector)
                          (coerce (system-metadata-source-control metadata) 'vector)))
          (flet ((validate-depends-on (json-list depends-on-list)
                   (assert (= (length json-list)
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
