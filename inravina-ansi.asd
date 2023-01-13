(asdf:defsystem #:inravina-ansi
  :description "ANSI terminal style for Inravina"
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/yitzchak/Inravina/"
  :bug-tracker "https://github.com/yitzchak/Inravina/issues"
  :depends-on (#:inravina)
  :components ((:module code
                :serial t
                :components ((:module ansi
                              :serial t
                              :components ((:file "packages")
                                           (:file "ansi")))))))
