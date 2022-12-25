(asdf:defsystem #:inravina
  :description "A portable and extensible Common Lisp pretty printer."
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/yitzchak/Inravina/"
  :bug-tracker "https://github.com/yitzchak/Inravina/issues"
  :in-order-to ((asdf:test-op (asdf:test-op #:inravina/test)))
  :depends-on ((:feature (:not :sicl) #:trivial-gray-streams)
               #:incless/core
               #:trivial-stream-column
               (:feature :sbcl #:sb-introspect))
  :components ((:module code
                :serial t
                :components ((:file "packages")
                             (:file "types")
                             (:file "interface")
                             (:file "client")
                             (:file "logical-block")
                             (:file "list-printers")
                             (:file "form-printers")
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

(asdf:defsystem #:inravina/extrinsic
  :description "Inravina/extrinsic"
  :author "Tarn W. Burton"
  :license "MIT"
  :components ((:module code
                :components ((:module extrinsic
                              :serial t
                              :components ((:file "packages")
                                           (:file "print")))))))

(asdf:defsystem #:inravina/intrinsic
  :description "Inravina/intrinsic"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on (:inravina :trivial-package-locks)
  :components ((:module code
                :components ((:module intrinsic
                              :serial t
                              :components ((:file "packages")
                                           (:file "print")
                                           (:file "post")))))))

(asdf:defsystem #:inravina/ansi-term
  :description "Inravina/ansi-term"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on (:inravina/intrinsic)
  :components ((:module code
                :components ((:module ansi-term
                              :serial t
                              :components ((:file "packages")
                                           (:file "term")))))))
