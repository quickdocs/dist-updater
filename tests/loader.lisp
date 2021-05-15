(defpackage #:dist-updater-tests/loader
  (:use #:cl
        #:rove)
  (:import-from #:dist-updater/loader
                #:load-json
                #:latest-dist-version)
  (:import-from #:cl-ppcre
                #:scan))
(in-package #:dist-updater-tests/loader)

(deftest latest-dist-version
  (let ((latest-dist-version (latest-dist-version)))
    (ok (scan "^\\d{4}-\\d{2}-\\d{2}$" latest-dist-version))))

(deftest load-json
  (let ((dist-version (latest-dist-version)))
    (ok (progn (load-json dist-version)
               t))))
