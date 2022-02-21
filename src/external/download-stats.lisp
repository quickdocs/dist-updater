(defpackage #:dist-updater/external/download-stats
  (:use #:cl)
  (:import-from #:dist-updater/models
                #:project-download-stats)
  (:import-from #:dist-updater/db
                #:with-connection)
  (:import-from #:dist-updater/http
                #:fetch)
  (:import-from #:cl-dbi)
  (:import-from #:sxql
                #:delete-from
                #:where)
  (:import-from #:mito)
  (:import-from #:split-sequence
                #:split-sequence)
  (:export #:fetch-download-stats
           #:load-download-stats))
(in-package #:dist-updater/external/download-stats)

(defun current-year-month ()
  ;; GMT
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore sec min hour day))
    (values year month)))

(defun download-stats-url (year month)
  (format nil "https://www.quicklisp.org/stats/~D/~:*~D-~2,'0D.csv" year month))

(defun read-line-values (stream &key (separator #\|) eof-value)
  (let* ((eof '#:eof)
         (line (read-line stream nil eof)))
    (if (eq line eof)
        eof-value
        (split-sequence separator line :count 3))))

(defun read-all-lines (stream)
  (let ((eof '#:eof))
    (loop for line = (read-line-values stream :eof-value eof)
          until (eq line eof)
          collect line)))

(defun fetch-download-stats (year month)
  (let* ((url (download-stats-url year month))
         (body (fetch url
                      :want-stream t
                      :use-connection-pool nil)))
    (read-all-lines body)))

(defun delete-download-stats (year month)
  (mito:execute-sql
    (delete-from :project_download_stats
      (where (:= :date (format nil "~4D-~2,'0D-01" year month))))))

(defun load-download-stats ()
  (multiple-value-bind (year month)
      (current-year-month)
    (format t "~&Fetching download stats at ~4D-~2,'0D...~%" year month)
    (let* ((lines (fetch-download-stats year month))
           (header-line (first lines))
           (data-lines (rest lines)))
      (declare (ignore header-line))
      (with-connection
        (dbi:with-transaction mito:*connection*
          (delete-download-stats year month)
          (dolist (line data-lines)
            (destructuring-bind (date download-count project-name)
                line
              (mito:create-dao 'project-download-stats
                               :date date
                               :download-count download-count
                               :project-name project-name)))))
      (format t "~&Loaded ~D rows.~%"
              (length data-lines)))))
