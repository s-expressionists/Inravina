(in-package #:asdf-user)

(defsystem "inravina-extension-extrinsic"
  :description "Inravina/extrinsic"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on ("incless-extension-extrinsic"
               "inravina-extension")
  :components ((:module "code"
                :pathname "code/extension-extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
