(in-package #:asdf-user)

(defsystem "inravina-extension-intrinsic"
  :description "Inravina-intrinsic"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on ("incless-extension-intrinsic"
               "inravina-extension")
  :components ((:module "code"
                :pathname "code/extension-intrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
