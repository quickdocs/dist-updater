(defpackage :dist-updater/json-db-class
  (:use :cl
        :alexandria)
  (:import-from :dist-updater/db
                :ensure-connection)
  (:import-from :mito)
  (:import-from :cl-change-case)
  (:export :define-json-db-class
           :gen-json-tables
           :migrate-all-json-tables
           :convert-json-aux
           :convert-json))
(in-package :dist-updater/json-db-class)

(defvar *json-db-classes* '())

(defun json-class-direct-slots (class-name)
  (get class-name 'direct-slots))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun construct-accessor (class-name slot-name)
    (symbolicate class-name '- slot-name)))

(defmacro define-json-db-class (class-name direct-superclasses direct-slots
                                &rest defclass-options)
  (let ((abstract (assoc-value defclass-options :abstract))
        (defclass-options (remove :abstract defclass-options :key #'first)))
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
         (:auto-pk :uuid)
         ,@defclass-options))))

(defun slot-to-key-name (slot-name)
  (cl-change-case:snake-case (string slot-name)))

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

(defun json-table-definitions ()
  (loop :for name :in (reverse *json-db-classes*)
        :append (mito:table-definition name)))

(defun gen-json-tables ()
  (mapc #'mito:execute-sql
        (json-table-definitions)))

(defun drop-all-json-tables ()
  (ensure-connection)
  (dolist (name *json-db-classes*)
    (ignore-errors
      (mito:execute-sql
       (sxql:drop-table
        (make-keyword
         (cl-change-case:snake-case
          (string name))))))))

(defun migrate-all-json-tables ()
  (dolist (name *json-db-classes*)
    (mito:migrate-table name)))
