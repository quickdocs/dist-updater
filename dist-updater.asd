(defsystem "dist-updater"
  :class :package-inferred-system
  :version "0.1.0"
  :depends-on ("dist-updater/main")
  :pathname "src"
  :in-order-to ((test-op (test-op "dist-updater-tests"))))

(defsystem "dist-updater/command"
  :depends-on ("dist-updater/command")
  :build-operation "program-op"
  :build-pathname "dist-updater"
  :entry-point "dist-updater/command::main")
