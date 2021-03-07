(defpackage :dist-updater/main
  (:use :cl :alexandria)
  (:import-from #:cl-store)
  (:import-from #:yason)
  (:import-from #:cl-change-case)
  (:import-from #:mito)
  (:import-from #:dexador))
(in-package :dist-updater/main)

;;;
(defvar *info-json-url* "https://storage.googleapis.com/quickdocs-dist/quicklisp/2021-02-28/info.json")
(defvar *releases-json-url* "https://storage.googleapis.com/quickdocs-dist/quicklisp/2021-02-28/releases.json")

(defun parse-json (json)
  (let ((hash-table (yason:parse json))
        (new-hash-table (make-hash-table)))
    (maphash (lambda (system-name url)
               (setf (gethash (make-keyword (string-upcase system-name))
                              new-hash-table)
                     url))
             hash-table)
    new-hash-table))

(defvar *fetch-cache* (make-hash-table :test 'equal))

(defparameter *fetch-use-cache* t)

(defun fetch (url)
  (if *fetch-use-cache*
      (or (gethash url *fetch-cache*)
          (setf (gethash url *fetch-cache*)
                (dex:get url)))
      (dex:get url)))

(defun save-fetch-cache ()
  (cl-store:store *fetch-cache* #p"fetch.out"))

(defun load-fetch-store ()
  (setf *fetch-cache* (cl-store:restore #p"fetch.out")))

(defun fetch-releases (&optional (url *releases-json-url*))
  (parse-json (fetch url)))

;;;
(defmacro with-transaction* ((&key (rollback t)) &body body)
  `(dbi:with-transaction mito:*connection*
     (let ((mito:*mito-logger-stream* *standard-output*))
       ,@body
       (when ,rollback (dbi:rollback mito:*connection*)))))

(defun connect-db ()
  (mito:connect-toplevel :postgres :database-name "quickdocs"))

;;;
(defvar *json-db-classes* '())

(defun json-class-direct-slots (class-name)
  (get class-name 'direct-slots))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun construct-accessor (class-name slot-name)
    (symbolicate class-name '- slot-name)))

(defmacro define-json-db-class (class-name direct-superclasses direct-slots &key abstract)
  `(progn
     (pushnew ',class-name *json-db-classes*)
     (setf (get ',class-name 'direct-slots) ',direct-slots)
     (setf (get ',class-name 'direct-superclasses) ',direct-superclasses)
     ,@(when abstract
         `((setf (get ',class-name 'abstract) t)))
     (mito:deftable ,class-name (,@direct-superclasses)
       ,(loop :for (slot-name . options) :in direct-slots
              :collect `(,slot-name ,@options
                                    :accessor ,(construct-accessor class-name slot-name)
                                    ,@(if (getf options :relational-type)
                                          (list :ghost t))))
       (:auto-pk :uuid))))

(defun slot-to-key-name (slot-name)
  (change-case:snake-case (string slot-name)))

(defgeneric convert-json-aux (class-name initargs))
(defmethod convert-json-aux (class-name initargs)
  (apply #'mito:create-dao class-name initargs))

(defun array-col-type-p (col-type)
  (and (keywordp col-type)
       (ppcre:register-groups-bind (scalar-type) ("(.*)\\[\\]$" (string col-type))
         scalar-type)))

(defun convert-json-1 (class-name json parent-class)
  (let ((initargs
          (loop :for (slot-name . options) :in (json-class-direct-slots class-name)
                :unless (getf options :relational-type)
                :collect (make-keyword (string slot-name)) :and
                :collect (cond ((eq (getf options :col-type)
                                    (type-of parent-class))
                                parent-class)
                               ((array-col-type-p (getf options :col-type))
                                (coerce (gethash (slot-to-key-name slot-name) json) 'vector))
                               (t
                                (gethash (slot-to-key-name slot-name) json))))))
    (convert-json-aux class-name initargs)))

(defun convert-json (class-name json &optional parent-class)
  (let ((dao (convert-json-1 class-name json parent-class)))
    (loop :for (slot-name . options) :in (json-class-direct-slots class-name)
          :do (when (getf options :relational-type)
                (setf (slot-value dao slot-name)
                      (let* ((relational-name (getf options :relational-type))
                             (value (gethash (slot-to-key-name slot-name) json)))
                        (if (eq 'list (getf options :type))
                            (loop :for elt :in value
                                  :collect (convert-json relational-name elt dao))
                            (when value
                              (convert-json relational-name
                                            value
                                            dao)))))))
    dao))

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
  :abstract t)

(defmethod convert-json-aux :around ((class-name (eql 'abstract-metadata-depends-on)) initargs)
  (destructuring-bind (&key name version feature) initargs
    (call-next-method class-name
                      (list :name name
                            :version version
                            :feature (prin1-to-string feature)))))

(define-json-db-class metadata-defsystem-depends-on (abstract-metadata-depends-on) ())
(define-json-db-class metadata-depends-on (abstract-metadata-depends-on) ())
(define-json-db-class metadata-weekly-depends-on (abstract-metadata-depends-on) ())

(defun json-table-definitions ()
  (loop :for name :in (reverse *json-db-classes*)
        :append (mito:table-definition name)))

(defun gen-json-tables ()
  (mapc #'mito:execute-sql
        (json-table-definitions)))

(defun drop-all-json-tables ()
  (dolist (name *json-db-classes*)
    (ignore-errors
      (mito:execute-sql
       (sxql:drop-table
        (make-keyword
         (change-case:snake-case
          (string name))))))))

(defun migrate-all-json-tables ()
  (dolist (name *json-db-classes*)
    (mito:migrate-table name)))

;;;
(defun ensure-connection ()
  (or mito:*connection*
      (connect-db)))

(defvar *release-table* (make-hash-table))

(defmethod print-object ((object hash-table) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (hash-table-alist object) stream)))

(defun download-all-releases ()
  (let ((release-table (make-hash-table)))
    (maphash (lambda (name url)
               (print (cons name url))
               (let ((yason (yason:parse (dex:get url))))
                 (setf (gethash name release-table) yason)))
             (fetch-releases))
    (setf *release-table* release-table)))

(defun dump-all-releases (*release-table*)
  (cl-store:store *release-table* #p"releases.out"))

(defun load-all-releases ()
  (setf *release-table* (cl-store:restore #p"releases.out")))

(defun create-releases-db ()
  (ensure-connection)
  (maphash (lambda (name yason)
             (defparameter $name name)
             (defparameter $yason yason)
             (convert-json 'release yason))
           *release-table*))

(defun create-system-db (systems)
  (maphash (lambda (system-name system)
             (defparameter $system-name system-name)
             (defparameter $system system)
             (convert-json 'system system))
           systems))

(defun create-all-systems-db ()
  (dolist (release (mito:select-dao 'release))
    (let ((url (release-systems-metadata-url release)))
      (create-system-db (yason:parse (fetch url))))))

(defun main ()
  (ensure-connection)
  (download-all-releases)
  ;; (dump-all-releases)
  (create-releases-db)
  (create-all-systems-db)
  )
