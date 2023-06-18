(in-package #:inravina-extrinsic)

(defclass extrinsic-client (incless-extrinsic:extrinsic-client)
  ())

(inravina:define-interface extrinsic-client)

(setf incless-extrinsic:*client* *client*)

(defmethod incless:write-object ((client extrinsic-client) object stream)
  (multiple-value-bind (func presentp)
      (and *print-pretty*
           (inravina:pprint-dispatch client *print-pprint-dispatch* object))
    (if presentp
        (funcall func stream object)
        (call-next-method)))
  object)
