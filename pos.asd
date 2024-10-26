(defsystem pos
  :author "April & May"
  :license "0BSD"
  :depends-on (alexandria for-loop log4cl)
  :components ((:file "package")
               (:file "conllu")
               (:file "core")))