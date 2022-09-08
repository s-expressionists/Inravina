(in-package #:inravina/ext)

#+abcl
(trivial-package-locks:with-unlocked-packages (:system)
  (defun system::%print-object (object stream)
    (multiple-value-bind (func presentp)
        (and *print-pretty*
             (pprint-dispatch object))
      (if presentp
         (funcall func stream object)
         (system::output-ugly-object object stream)))))

#+sbcl
(trivial-package-locks:with-unlocked-packages (:sb-pretty)
  (defun sb-pretty:output-pretty-object (stream fun object)
    (funcall fun stream object)))
