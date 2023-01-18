(asdf:defsystem #:inravina-extrinsic
  :description "Inravina/extrinsic"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on (#:incless-extrinsic
               #:inravina)
  :components ((:module code
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "print")))))

