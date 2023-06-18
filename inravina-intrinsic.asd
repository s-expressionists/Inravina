(in-package #:asdf-user)

(defsystem "inravina-intrinsic"
  :description "Inravina-intrinsic"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on ("incless-intrinsic"
               "inravina")
  :components ((:module "code"
                :pathname "code/intrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "print")))))
