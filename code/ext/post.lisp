(in-package #:inravina/ext)

#+sbcl
(trivial-package-locks:with-unlocked-packages (:sb-pretty)
  (defun output-pretty-object (stream fun object)
    (print "a")
    (funcall fun stream object)))
