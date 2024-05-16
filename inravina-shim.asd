(in-package #:asdf-user)

(defsystem "inravina-shim"
  :description "Inravina-shim"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on ("incless-native"
               "inravina"
               "trivial-package-locks")
  :in-order-to ((test-op (test-op "inravina-shim/test")))
  :components ((:module "code"
                :pathname "code/shim/"
                :serial t
                :components ((:file "packages")
                             (:file "print")))))
