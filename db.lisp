(defpackage :dist-updater/db
  (:use :cl)
  (:import-from :cl-dbi)
  (:import-from :mito)
  (:export :with-transaction*
           :connect-db
           :ensure-connection))
(in-package :dist-updater/db)

(defmacro with-transaction* ((&key (rollback t)) &body body)
  `(dbi:with-transaction mito:*connection*
     (let ((mito:*mito-logger-stream* *standard-output*))
       (prog1 (progn ,@body)
         (when ,rollback (dbi:rollback mito:*connection*))))))

(defun connect-db ()
  (mito:connect-toplevel :postgres :database-name "quickdocs"))

(defun ensure-connection ()
  (or mito:*connection*
      (connect-db)))
