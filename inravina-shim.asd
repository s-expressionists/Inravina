(asdf:defsystem #:inravina-shim
  :description "Inravina-shim"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on (:incless-native :inravina-intrinsic)
  :components ((:module code
                :components ((:module shim
                              :serial t
                              :components ((:file "packages")
                                           (:file "shim")))))))
