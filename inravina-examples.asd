(in-package #:asdf-user)

(defsystem "inravina-examples"
  :description "Inravina Examples"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on ("cl-pdf"
               "inravina-extrinsic")
  :components ((:module "code"
                :pathname "code/examples/"
                :serial t
                :components ((:file "packages")
                             (:file "pdf")))))

(defsystem "inravina-examples/cst"
  :depends-on ("concrete-syntax-tree"
               "inravina-extrinsic")
  :components ((:module "code"
                :pathname "code/examples/cst"
                :serial t
                :components ((:file "packages")
                             (:file "cst")))))
           
