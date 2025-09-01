(in-package #:inravina-intrinsic)

(defclass intrinsic-client (incless-intrinsic:intrinsic-client)
  ())

(defclass intrinsic-client-impl
    (intrinsic-client quaviver/schubfach:client)
  ())

(inravina:define-interface :client-form incless-intrinsic:*client*
                           :client-class intrinsic-client
                           :intrinsic t)

(setf incless-intrinsic:*client* (make-instance 'intrinsic-client-impl))

(defmethod incless:write-object ((client intrinsic-client) object stream)
  (multiple-value-bind (func presentp)
      (and *print-pretty*
           (inravina:pprint-dispatch client *print-pprint-dispatch* object))
    (if presentp
        (funcall func stream object)
        (call-next-method)))
  object)
