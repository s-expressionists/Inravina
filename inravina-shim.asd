(asdf:defsystem "inravina-shim"
  :description "Inravina-shim"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on ("incless-native" "inravina-intrinsic")
  :in-order-to ((asdf:test-op (asdf:test-op "inravina-shim/test")))
  :components ((:module code
                :pathname "code/shim/"
                :serial t
                :components ((:file "packages")
                             (:file "shim")))))

(asdf:defsystem "inravina-shim/test"
  :description "ANSI Test system for Inravina"
  :license "MIT"
  :author "Tarn W. Burton"
  :maintainer "Tarn W. Burton"
  :depends-on ("alexandria" "inravina-shim")
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :inravina-shim/test :test))
  :components ((:module code
                :pathname "code/shim/test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")))))
