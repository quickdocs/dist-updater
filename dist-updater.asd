(defsystem "dist-updater"
  :class :package-inferred-system
  :version "0.1.3"
  :depends-on ("dist-updater/main"
               "dbd-postgres")
  :pathname "src"
  :in-order-to ((test-op (test-op "dist-updater-tests"))))

(defsystem "dist-updater/exectable"
  :depends-on ("dist-updater/command")
  :build-operation "program-op"
  :build-pathname "dist-updater"
  :entry-point "dist-updater/command::main")
