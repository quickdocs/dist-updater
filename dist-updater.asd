(defsystem "dist-updater"
  :class :package-inferred-system
  :depends-on ("dist-updater/main")
  :pathname "src")

(defsystem "dist-updater/command"
  :depends-on ("dist-updater")
  :build-operation "program-op"
  :build-pathname "dist-updater"
  :entry-point "dist-updater::main")
