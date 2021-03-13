(defpackage :dist-updater/fetch
  (:use :cl :alexandria)
  (:import-from :quri)
  (:import-from :dexador)
  (:import-from :yason)
  (:export :fetch
           :fetch-releases))
(in-package :dist-updater/fetch)

(defun url-to-local-pathname (url)
  (let* ((path (quri:uri-path (quri:uri url)))
         (pathname (asdf:system-relative-pathname :dist-updater (pathname (string-left-trim "/" path)))))
    pathname))

(defun fetch (url)
  (let ((pathname (url-to-local-pathname url)))
    (cond ((uiop:file-exists-p pathname)
           (values (read-file-into-string pathname) t))
          (t
           (ensure-directories-exist pathname)
           (values (write-string-into-file (dex:get url) pathname :if-exists :supersede)
                   nil)))))
