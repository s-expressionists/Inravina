(in-package #:asdf-user)

(defsystem "inravina-extrinsic"
  :description "Inravina/extrinsic"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on ("incless-extrinsic"
               "inravina")
  :in-order-to ((asdf:test-op (asdf:test-op #:inravina-extrinsic/test)))
  :components ((:module "code"
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "print")))))

(asdf:defsystem "inravina-extrinsic/test"
  :description "Extrinsic testing interface to Inravina."
  :license "MIT"
  :author "Tarn W. Burton"
  :homepage "https://github.com/s-expressionists/Inravina"
  :bug-tracker "https://github.com/s-expressionists/Inravina/issues"
  :depends-on ("inravina-extrinsic"
               "ansi-test-harness")
  :perform (asdf:test-op (op c)
             (symbol-call :inravina-extrinsic/test :test))
  :components ((:module code
                :pathname "code/extrinsic/test/"
                :serial t
                :components ((:file "packages")
                             (:file "ansi-test")
                             (:static-file "expected-failures.sexp")))))
