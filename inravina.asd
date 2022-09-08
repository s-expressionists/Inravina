(asdf:defsystem #:inravina
  :description "A portable and extensible Common Lisp pretty printer."
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://yitzchak.github.io/Inravina/"
  :bug-tracker "https://github.com/yitzchak/Inravina/issues"
  :in-order-to ((asdf:test-op (asdf:test-op #:inravina/test)))
  :depends-on ((:feature (:not :sicl) #:trivial-gray-streams)
               #:trivial-stream-column
               (:feature :sbcl #:sb-introspect))
  :components
    ((:module code
      :serial t
      :components
        ((:file "packages")
         (:file "interface")
         (:file "types")
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
  :components
    ((:module code
      :components
      ((:module test
        :serial t
        :components
          ((:file "packages")
           (:file "utilities")
           (:file "dispatch")
           (:file "pprint-tab")
           (:file "list-printers")
           (:file "form-printers")))))))

(asdf:defsystem #:inravina/ext.extrinsic
  :description "Inravina/ext extrinsic"
  :author "Tarn W. Burton"
  :license "MIT"
  :components ((:module code
                :components ((:module ext
                              :serial t
                              :components ((:file "packages-extrinsic")
                                           (:file "print")))))))

(asdf:defsystem #:inravina/ext.intrinsic
  :description "Inravina/ext intrinsic"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on (:inravina)
  :components ((:module code
                :components ((:module ext
                              :serial t
                              :components ((:file "packages-intrinsic")
                                           (:file "print")))))))

(asdf:defsystem #:inravina/ext.shim
  :description "Inravina/ext shim"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on (:inravina :trivial-package-locks)
  :components ((:module code
                :components ((:module ext
                              :serial t
                              :components ((:file "packages-intrinsic")
                                           (:file "shim")
                                           (:file "print")
                                           (:file "post")))))))
