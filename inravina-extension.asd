(asdf:defsystem "inravina-extension"
  :description "A portable and extensible Common Lisp pretty printer."
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/s-expressionists/Inravina/"
  :bug-tracker "https://github.com/s-expressionists/Inravina/issues"
  :depends-on ("inravina")
  :components ((:module code
                :pathname "code/extension/"
                :serial t
                :components ((:file "packages")
                             (:file "implementation")))))
