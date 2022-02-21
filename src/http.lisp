(defpackage #:dist-updater/http
  (:use #:cl)
  (:import-from #:dexador)
  (:import-from #:yason)
  (:export #:fetch
           #:fetch-json))
(in-package #:dist-updater/http)

(defun fetch (url &rest dex-args)
  (let ((retry-request (dex:retry-request 5 :interval 3)))
    (handler-bind ((dex:http-request-service-unavailable retry-request))
      (apply #'dex:get url dex-args))))

(defun fetch-json (url &rest dex-args)
  (yason:parse (apply #'fetch url dex-args)))
