(asdf:defsystem #:inravina-intrinsic
  :description "Inravina-intrinsic"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on (:inravina :trivial-package-locks)
  :components ((:module code
                :components ((:module intrinsic
                              :serial t
                              :components ((:file "packages")
                                           (:file "print")
                                           (:file "post")))))))
