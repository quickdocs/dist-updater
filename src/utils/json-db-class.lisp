(defpackage :dist-updater/utils/json-db-class
  (:use :cl
        :alexandria)
  (:import-from :mito)
  (:import-from :cl-change-case)
  (:export :define-json-db-class
           :convert-json-aux
           :convert-json
           :generate-json-tables
           :migrate-all-json-tables))
(in-package :dist-updater/utils/json-db-class)

(defvar *json-db-classes* '())

(defun json-class-direct-superclasses (class-name)
  (get class-name 'direct-superclasses))

(defun json-class-direct-slots (class-name)
  (get class-name 'direct-slots))

(defun json-class-slots (class-name)
  (append (json-class-direct-slots class-name)
          (mapcan #'json-class-slots (json-class-direct-superclasses class-name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun construct-accessor (class-name slot-name)
    (symbolicate class-name '- slot-name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun construct-1-n-def-accessors (class-name direct-slots)
    (with-unique-names (instance)
      (loop :for (slot-name . options) :in direct-slots
            :when (and (getf options :relational-type)
                       (eq (getf options :type) 'list))
            :collect `(defmethod ,(construct-accessor class-name slot-name) ((,instance ,class-name))
                        (if (slot-boundp ,instance ',slot-name)
                            (slot-value ,instance ',slot-name)
                            (setf (slot-value ,instance ',slot-name)
                                  (mito:retrieve-dao ',(getf options :relational-type)
                                                     ,(make-keyword (symbolicate class-name '-id))
                                                     (mito:object-id ,instance)))))))))

(defmacro define-json-db-class (class-name direct-superclasses direct-slots
                                &rest defclass-options)
  (let ((abstract (assoc-value defclass-options :abstract))
        (defclass-options (remove :abstract defclass-options :key #'first)))
    `(progn
       (unless (member ',class-name *json-db-classes*)
         (nconcf *json-db-classes* (list ',class-name)))
       (setf (get ',class-name 'direct-slots) ',direct-slots)
       (setf (get ',class-name 'direct-superclasses) ',direct-superclasses)
       ,@(when abstract
           `((setf (get ',class-name 'abstract) t)))
       (mito:deftable ,class-name (,@direct-superclasses)
         ,(loop :for (slot-name . options) :in direct-slots
                :collect `(,slot-name ,@options
                                      :accessor ,(construct-accessor class-name slot-name)
                                      ,@(unless (getf options :col-type)
                                          (list :ghost t))))
         (:auto-pk :uuid)
         ,@defclass-options)
       ,@(construct-1-n-def-accessors class-name direct-slots))))

(defun generate-defpackage-export-1 (class-name)
  (cons (make-keyword class-name)
        (mapcar (lambda (slot)
                  (make-keyword (construct-accessor class-name (first slot))))
                (json-class-direct-slots class-name))))

(defun generate-defpackage-export ()
  (let ((*print-case* :downcase)
        (*print-right-margin* 0))
    (pprint (mapcan #'generate-defpackage-export-1
                    *json-db-classes*))))

(defun slot-to-key-name (slot-name)
  (cl-change-case:snake-case (string slot-name)))

(defgeneric convert-json-aux (dao))
(defmethod convert-json-aux (dao)
  dao)

(defun array-col-type-p (col-type)
  (let ((col-type (mito.dao.mixin::parse-col-type col-type)))
    (and (keywordp col-type)
         (ppcre:register-groups-bind (scalar-type) ("(.*)\\[\\]$" (string col-type))
           scalar-type))))

(defun convert-json-1 (class-name json parent-class)
  (let ((initargs
          (loop :for (slot-name . options) :in (json-class-slots class-name)
                :when (getf options :col-type)
                :collect (make-keyword (string slot-name)) :and
                :collect (cond ((eq (getf options :col-type)
                                    (type-of parent-class))
                                parent-class)
                               ((array-col-type-p (getf options :col-type))
                                (let ((value (gethash (slot-to-key-name slot-name) json)))
                                  (if (listp value)
                                      (coerce value 'vector)
                                      value)))
                               (t
                                (gethash (slot-to-key-name slot-name) json))))))
    (convert-json-aux (apply #'make-instance class-name initargs))))

(defun convert-json (class-name json &optional parent-class)
  (let ((dao (convert-json-1 class-name json parent-class)))
    (loop :for (slot-name . options) :in (json-class-slots class-name)
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
    (mito:save-dao dao)
    dao))

(defun json-table-definitions ()
  (loop :for name :in *json-db-classes*
        :append (mito:table-definition name)))

(defun generate-json-tables ()
  (mapc #'mito:execute-sql
        (json-table-definitions)))

(defun drop-all-json-tables ()
  (dolist (name *json-db-classes*)
    (ignore-errors
      (mito:execute-sql
       (sxql:drop-table
        (make-keyword
         (cl-change-case:snake-case
          (string name))))))))

(defun delete-all-records ()
  (dolist (name *json-db-classes*)
    (mito:delete-by-values name)))

(defun migrate-all-json-tables ()
  (dolist (name *json-db-classes*)
    (mito:migrate-table name)))
