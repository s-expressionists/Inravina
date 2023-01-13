(asdf:defsystem #:inravina-extrinsic
  :description "Inravina/extrinsic"
  :author "Tarn W. Burton"
  :license "MIT"
  :components ((:module code
                :components ((:module extrinsic
                              :serial t
                              :components ((:file "packages")
                                           (:file "print")))))))

