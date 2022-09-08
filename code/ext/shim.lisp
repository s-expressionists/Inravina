(in-package #:inravina/ext)

(defclass shim-client (inravina:client)
  ())

;(setf inravina:*client* (make-instance 'shim-client))

(defmethod inravina:write-object ((client inravina:client) stream object)
  (declare (ignorable client))
  #+clasp (sys:write-object object stream)
  #+sbcl (sb-impl::output-object object stream))

#+clasp
(defmethod inravina:handle-circle ((client inravina:client) stream object function)
  #+clasp (sys::write-object-with-circle object stream
                                         (lambda (object stream)
                                           (funcall function stream object))))