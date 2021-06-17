(defpackage #:dist-updater/models
  (:use #:cl)
  (:import-from #:mito
                #:deftable)
  (:export #:dist
           #:dist-name
           #:dist-version
           #:dist-system-index-url
           #:dist-release-index-url
           #:dist-archive-base-url
           #:dist-distinfo-subscription-url
           #:dist-canonical-distinfo-url
           #:dist-provided-releases-count
           #:dist-provided-releases-url
           #:dist-extract-errors-url

           #:release
           #:release-dist-name
           #:release-dist-version
           #:release-name
           #:release-archive-url
           #:release-archive-size
           #:release-archive-content-sha1
           #:release-prefix
           #:release-systems-metadata-url
           #:release-readme-url
           #:release-upstream-url

           #:dist-release
           #:dist-release-dist
           #:dist-release-release

           #:readme-file
           #:readme-file-release
           #:readme-file-filename
           #:readme-file-extension
           #:readme-file-content

           #:system
           #:system-release
           #:system-name
           #:system-long-name
           #:system-filename
           #:system-version
           #:system-description
           #:system-long-description
           #:system-authors
           #:system-maintainers
           #:system-mailto
           #:system-license
           #:system-homepage
           #:system-bug-tracker
           #:system-source-control-url

           #:system-dependency
           #:system-dependency-system
           #:system-dependency-name
           #:system-dependency-type
           #:system-dependency-version
           #:system-dependency-feature))
(in-package #:dist-updater/models)

(deftable dist ()
  ((name :col-type (:varchar 32))
   (version :col-type (:char 10))
   (system-index-url :col-type (:varchar 128))
   (release-index-url :col-type (:varchar 128))
   (archive-base-url :col-type (:varchar 128))
   (distinfo-subscription-url :col-type (:varchar 128))
   (canonical-distinfo-url :col-type (:varchar 128))
   (provided-releases-count :col-type :integer)
   (provided-releases-url :col-type (:varchar 128))
   (extract-errors-url :col-type (:varchar 128)))
  (:unique-keys (name version)))

(deftable release ()
  ((dist-name :col-type (:varchar 32))
   (dist-version :col-type (:char 10))
   (name :col-type (:varchar 64))
   (archive-url :col-type :text)
   (archive-size :col-type :integer)
   (archive-content-sha1 :col-type (:varchar 40))
   (prefix :col-type :text)
   (systems-metadata-url :col-type :text)
   (readme-url :col-type :text)
   (upstream-url :col-type :text))
  (:unique-keys (dist-name dist-version name)))

(deftable dist-release ()
  ((dist :col-type dist)
   (release :col-type release))
  (:primary-key dist release)
  (:record-timestamps nil))

(deftable readme-file ()
  ((release :col-type release)
   (filename :col-type (:varchar 64))
   (content :col-type :text))
  (:unique-keys (release filename)))

(deftable system ()
  ((release :col-type release)
   (name :col-type (:varchar 64))
   (filename :col-type (:varchar 80))
   (long-name :col-type (or :null :text))
   (version :col-type (or :null :text))
   (description :col-type (or :null :text))
   (long-description :col-type (or :null :text))
   (authors :col-type :text[])
   (maintainers :col-type :text[])
   (mailto :col-type (or :null :text))
   (license :col-type (or :null :text))
   (homepage :col-type (or :null :text))
   (bug-tracker :col-type (or :null :text))
   (source-control-url :col-type (or :null :text)))
  (:unique-keys (release name)))

(deftable system-dependency ()
  ((system :col-type system)
   (type :col-type (:varchar 12)) ;; one of 'normal', 'weakly' or 'defsystem'
   (name :col-type (:varchar 64))
   (version :col-type (or :null :text))
   (feature :col-type (or :null :text)))
  (:unique-keys (system type name)))
