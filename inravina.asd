(asdf:defsystem #:inravina
  :description "A portable and extensible Common Lisp pretty printer."
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://yitzchak.github.io/Inravina/"
  :bug-tracker "https://github.com/yitzchak/Inravina/issues"
  :depends-on (#:trivial-gray-streams)
  :components
    ((:module code
      :serial t
      :components
        ((:file "packages")
         (:file "interface")
         (:file "client")
         (:file "queue")
         (:file "dispatch")
         (:file "pretty-stream")
         (:file "implementation")))))
         
