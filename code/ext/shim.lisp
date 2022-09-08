(in-package #:inravina/ext)

(defclass shim-client (inravina:client)
  ())

;(setf inravina:*client* (make-instance 'shim-client))

(defmethod inravina:write-object ((client inravina:client) stream object)
  (declare (ignorable client))
  #+abcl (system:output-object object stream)
  #+(or clasp ecl) (sys:write-object object stream)
  #+sbcl (sb-impl::output-object object stream))

#+(or clasp ecl)
(defmethod inravina:handle-circle ((client inravina:client) stream object function)
  (sys::write-object-with-circle object stream
                                 (lambda (object stream)
                                   (funcall function stream object))))
