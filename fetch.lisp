(defpackage :dist-updater/fetch
  (:use :cl)
  (:import-from :dexador)
  (:import-from :yason)
  (:import-from :cl-store)
  (:export :fetch
           :fetch-releases))
(in-package :dist-updater/fetch)

(defparameter *info-json-url* "https://storage.googleapis.com/quickdocs-dist/quicklisp/2021-02-28/info.json")
(defparameter *releases-json-url* "https://storage.googleapis.com/quickdocs-dist/quicklisp/2021-02-28/releases.json")

(defparameter *fetch-use-cache* t)

(defvar *fetch-cache* (make-hash-table :test 'equal))

(defun parse-json (json)
  (let ((hash-table (yason:parse json))
        (new-hash-table (make-hash-table)))
    (maphash (lambda (system-name url)
               (setf (gethash (make-keyword (string-upcase system-name))
                              new-hash-table)
                     url))
             hash-table)
    new-hash-table))

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
