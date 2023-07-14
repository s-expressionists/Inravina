(in-package #:asdf-user)

(defsystem "inravina-native"
  :description "Inravina-native"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on ("inravina")
  :components ((:module "code"
                :pathname "code/native/"
                :serial t
                :components ((:file "packages")
                             (:file "print")))))
