(defpackage :dist-updater/db-classes
  (:use :cl
        :alexandria
        :dist-updater/utils/json-db-class)
  (:export
   :release
   :release-project-name
   :release-archive-url
   :release-archive-size
   :release-archive-content-sha1
   :release-prefix
   :release-systems
   :release-systems-metadata-url
   :release-readme-url
   :release-upstream-url
   :release-system
   :release-system-release
   :release-system-name
   :release-system-system-file-name
   :release-system-required-systems
   :system-metadata
   :system-metadata-name
   :system-metadata-long-name
   :system-metadata-version
   :system-metadata-description
   :system-metadata-long-description
   :system-metadata-author
   :system-metadata-maintainer
   :system-metadata-mailto
   :system-metadata-license
   :system-metadata-homepage
   :system-metadata-bug-tracker
   :system-metadata-source-control
   :system-metadata-defsystem-depends-on
   :system-metadata-depends-on
   :system-metadata-weakly-depends-on
   :abstract-metadata-depends-on
   :abstract-metadata-depends-on-system-metadata
   :abstract-metadata-depends-on-name
   :abstract-metadata-depends-on-version
   :abstract-metadata-depends-on-feature
   :metadata-defsystem-depends-on
   :metadata-depends-on
   :metadata-weakly-depends-on
   :system
   :system-name
   :system-system-file-name
   :system-required-systems
   :system-metadata
   :readme-file
   :readme-file-filename
   :readme-file-content
   :readme
   :readme-name
   :readme-readme-files))
(in-package :dist-updater/db-classes)

;;; release
(define-json-db-class release ()
  ((project-name :col-type :text)
   (archive-url :col-type :text)
   (archive-size :col-type :integer)
   (archive-content-sha1 :col-type (:varchar 40))
   (prefix :col-type :text)
   (systems :relational-type release-system :type list)
   (systems-metadata-url :col-type :text)
   (readme-url :col-type :text)
   (upstream-url :col-type :text)))

(define-json-db-class release-system ()
  ((release :col-type release)
   (name :col-type :text)
   (system-file-name :col-type :text)
   (required-systems :col-type :text[])))

;;; readme
(define-json-db-class readme ()
  ((name :col-type :text)
   (readme-files :relational-type readme-file :type list)))

(define-json-db-class readme-file ()
  ((readme :col-type readme)
   (filename :col-type :text)
   (content :col-type :text)))

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
           (etypecase x
             (list (coerce x 'vector))
             (string (vector x))
             (vector x))))
    (ensure-seq (if (and (or (consp value) (vectorp value))
                         (length= value 2)
                         (equal "quote" (elt value 0)))
                    (elt value 1)
                    value))))

(defun normalize-source-control (value)
  (typecase value
    (vector (normalize-source-control (coerce value 'list)))
    (cons (destructuring-bind (type url)
              value
            (normalize-array
              (list (or type "unknown") url))))
    (otherwise (normalize-array value))))

(defmethod convert-json-aux :around ((dao system-metadata))
  (setf (system-metadata-author dao)
        (normalize-array (system-metadata-author dao)))
  (setf (system-metadata-maintainer dao)
        (normalize-array (system-metadata-maintainer dao)))
  (setf (system-metadata-source-control dao)
        (normalize-source-control (system-metadata-source-control dao)))
  (setf (system-metadata-description dao)
        (if (consp (system-metadata-description dao))
            (first (system-metadata-description dao))
            (system-metadata-description dao)))
  (call-next-method dao))

;;; system.metdata.depends_on
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
