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

;;; system
(define-json-db-class system ()
  ((name :col-type :text)
   (system-file-name :col-type :text)
   (required-systems :col-type :text[])
   (metadata :relational-type system-metadata :type system-metadata)))

(define-json-db-class system-metadata ()
  ((system :col-type system)
   (name :col-type (or :null :text))
   (long-name :col-type (or :null :text))
   (version :col-type (or :null :text))
   (description :col-type (or :null :text))
   (long-description :col-type (or :null :text))
   (author :col-type (or :null :text[]))
   (maintainer :col-type (or :null :text[]))
   (mailto :col-type (or :null :text))
   (license :col-type (or :null :text))
   (homepage :col-type (or :null :text))
   (bug-tracker :col-type (or :null :text))
   (source-control :col-type (or :null :text[]))))

(defun normalize-array (value)
  (coerce (ensure-list (if (and (consp value)
                                (length= value 2)
                                (equal "quote" (first value)))
                           (second value)
                           value))
          'vector))

(defmethod convert-json-aux :around ((class-name (eql 'system-metadata)) initargs)
  (destructuring-bind (&rest initargs &key author maintainer source-control description &allow-other-keys) initargs
    (call-next-method
     class-name
     (list* :author (normalize-array author)
            :maintainer (normalize-array maintainer)
            :source-control (normalize-array source-control)
            :description (if (consp description) (first description) description)
            (remove-from-plist initargs :author :maintainer :source-control :description)))))

(define-json-db-class abstract-metadata-depends-on ()
  ((name :col-type :text)
   (version :col-type (or :null :text))
   (feature :col-type :text) ; s-expression to string
   )
  (:abstract t))

(defmethod convert-json-aux :around ((class-name (eql 'abstract-metadata-depends-on)) initargs)
  (destructuring-bind (&key name version feature) initargs
    (call-next-method class-name
                      (list :name name
                            :version version
                            :feature (prin1-to-string feature)))))

(define-json-db-class metadata-defsystem-depends-on (abstract-metadata-depends-on) ())
(define-json-db-class metadata-depends-on (abstract-metadata-depends-on) ())
(define-json-db-class metadata-weekly-depends-on (abstract-metadata-depends-on) ())

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
(defvar $system)

(defun fetch-and-create-release-db ()
  (maphash (lambda (name url)
             (format t "~&fetch ~A, ~A~&" name url)
             (let ((json (yason:parse (fetch url))))
               (convert-json 'release json)))
           (fetch-releases)))

(defun create-system-db (systems)
  (maphash (lambda (system-name system)
             (setf $system-name system-name)
             (setf $system system)
             (convert-json 'system system))
           systems))

(defun create-all-systems-db ()
  (dolist (release (mito:select-dao 'release))
    (let ((url (release-systems-metadata-url release)))
      (create-system-db (yason:parse (fetch url))))))

(defun main ()
  (ensure-connection)
  (fetch-and-create-release-db)
  (create-all-systems-db)
  )
