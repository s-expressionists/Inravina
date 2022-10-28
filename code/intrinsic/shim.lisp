(in-package #:inravina/intrinsic)

(defclass client ()
  ())

(defmethod inravina:write-object (client stream object)
  (declare (ignorable client))
  #+abcl (system:output-object object stream)
  #+(or clasp ecl) (sys:write-object object stream)
  #+sbcl (sb-impl::output-object object stream))

#+(or clasp ecl)
(defmethod inravina:handle-circle (client stream object function)
  (sys::write-object-with-circle object stream
    (lambda (object stream)
      (funcall function stream object))))
