(defsystem "dist-updater"
  :class :package-inferred-system
  :depends-on ("dist-updater/main")
  :pathname "src"
  :in-order-to ((test-op (test-op "dist-updater-tests"))))

(defsystem "dist-updater/command"
  :depends-on ("dist-updater/command")
  :build-operation "program-op"
  :build-pathname "dist-updater"
  :entry-point "dist-updater/command::main")
