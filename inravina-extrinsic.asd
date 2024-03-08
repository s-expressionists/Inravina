(in-package #:asdf-user)

(defsystem "inravina-extrinsic"
  :description "Inravina/extrinsic"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on ("incless-extrinsic"
               "inravina")
  :in-order-to ((asdf:test-op (asdf:test-op #:inravina-extrinsic/ansi-test)))
  :components ((:module "code"
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "print")))))

(asdf:defsystem "inravina-extrinsic/unit-test"
  :description "Unit testing suite for Inravina."
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on ("alexandria"
               "inravina-extrinsic"
               "parachute")
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :inravina-extrinsic/unit-test))
  :components ((:module code
                :pathname "code/extrinsic/unit-test/"
                :serial t
                :components ((:file "packages")
                             (:file "utilities")
                             (:file "pprint-logical-block")))))

(asdf:defsystem "inravina-extrinsic/ansi-test"
  :description "ANSI testing suite to Inravina."
  :license "MIT"
  :author "Tarn W. Burton"
  :homepage "https://github.com/s-expressionists/Inravina"
  :bug-tracker "https://github.com/s-expressionists/Inravina/issues"
  :depends-on ("inravina-extrinsic"
               "ansi-test-harness")
  :perform (asdf:test-op (op c)
             (symbol-call :inravina-extrinsic/ansi-test :test))
  :components ((:module code
                :pathname "code/extrinsic/ansi-test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")
                             (:static-file "expected-failures.sexp")))))
