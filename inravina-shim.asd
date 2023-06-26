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

(defsystem "inravina-shim/test"
  :description "ANSI Test system for Inravina"
  :license "MIT"
  :author "Tarn W. Burton"
  :maintainer "Tarn W. Burton"
  :depends-on ("alexandria" "inravina-shim")
  :perform (test-op (op c)
             (symbol-call :inravina-shim/test :test))
  :components ((:module "code"
                :pathname "code/shim/test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")))
               (:module "expected-failures"
                :pathname "code/shim/test/expected-failures"
                :components ((:static-file "default.sexp")
                             (:static-file "abcl.sexp")
                             (:static-file "clasp.sexp")
                             (:static-file "cmucl.sexp")
                             (:static-file "ecl.sexp")))))
