(asdf:defsystem #:inravina
  :description "A portable and extensible Common Lisp pretty printer."
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/s-expressionists/Inravina/"
  :bug-tracker "https://github.com/s-expressionists/Inravina/issues"
  :in-order-to ((asdf:test-op (asdf:test-op #:inravina/test)))
  :depends-on ((:feature (:not :sicl) #:trivial-gray-streams)
               #:incless
               #:trivial-stream-column
               (:feature :sbcl (:require #:sb-introspect)))
  :components ((:module code
                :serial t
                :components ((:file "packages")
                             (:file "types")
                             (:file "interface")
                             (:file "logical-block")
                             (:file "list-printers")
                             (:file "form-printers")
                             (:file "object-printers")
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
  :components ((:module code
                :components ((:module test
                              :serial t
                              :components ((:file "packages")
                                           (:file "utilities")
                                           (:file "dispatch")
                                           (:file "pprint-tab")
                                           (:file "list-printers")
                                           (:file "form-printers")))))))
