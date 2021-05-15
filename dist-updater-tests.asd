(defsystem "dist-updater-tests"
  :depends-on ("dist-updater"
               "rove"
               "cl-ppcre")
  :pathname "tests"
  :components ((:file "loader"))
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
