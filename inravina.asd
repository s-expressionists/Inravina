(asdf:defsystem #:inravina
  :description "A portable and extensible Common Lisp pretty printer."
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://yitzchak.github.io/Inravina/"
  :bug-tracker "https://github.com/yitzchak/Inravina/issues"
  :in-order-to ((asdf:test-op (asdf:test-op #:inravina/test)))
  :depends-on (#:trivial-gray-streams)
  :components
    ((:module code
      :serial t
      :components
        ((:file "packages")
         (:file "interface")
         (:file "client")
         (:file "dispatch")
         (:file "pretty-stream")
         (:file "implementation")))))

(asdf:defsystem #:inravina/test
  :description "Test suite for Inravina"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on
    (:alexandria :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :inravina/test))
  :components
    ((:module code
      :components
      ((:module test
        :serial t
        :components
          ((:file "packages")
           (:file "dispatch")
           (:file "pprint-tab")
           (:file "pprint-list")))))))
         
