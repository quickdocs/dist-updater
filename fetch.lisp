(defpackage :dist-updater/fetch
  (:use :cl :alexandria)
  (:import-from :quri)
  (:import-from :dexador)
  (:import-from :yason)
  (:export :fetch
           :fetch-releases))
(in-package :dist-updater/fetch)

(defparameter *info-json-url* "https://storage.googleapis.com/quickdocs-dist/quicklisp/2021-02-28/info.json")
(defparameter *releases-json-url* "https://storage.googleapis.com/quickdocs-dist/quicklisp/2021-02-28/releases.json")

(defun parse-json (json)
  (let ((hash-table (yason:parse json))
        (new-hash-table (make-hash-table)))
    (maphash (lambda (system-name url)
               (setf (gethash (make-keyword (string-upcase system-name))
                              new-hash-table)
                     url))
             hash-table)
    new-hash-table))

(defun url-to-local-pathname (url)
  (let* ((path (quri:uri-path (quri:uri url)))
         (pathname (asdf:system-relative-pathname :dist-updater (pathname (string-left-trim "/" path)))))
    pathname))

(defun fetch (url)
  (let ((data (dex:get url))
        (pathname (url-to-local-pathname url)))
    (cond ((uiop:file-exists-p pathname)
           (values (read-file-into-string pathname) t))
          (t
           (ensure-directories-exist pathname)
           (values (write-string-into-file data pathname :if-exists :supersede)
                   nil)))))

(defun fetch-releases (&optional (url *releases-json-url*))
  (parse-json (fetch url)))
