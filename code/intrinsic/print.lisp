(in-package #:inravina-intrinsic)

(defclass intrinsic-client (incless-intrinsic:intrinsic-client)
  ())

(trivial-package-locks:with-unlocked-packages (:common-lisp)
  (inravina:define-interface intrinsic-client))

(setf incless-intrinsic:*client* *client*)

(defmethod incless:write-object ((client intrinsic-client) object stream)
  (multiple-value-bind (func presentp)
      (and *print-pretty*
           (inravina:pprint-dispatch client *print-pprint-dispatch* object))
    (if presentp
        (funcall func stream object)
        (call-next-method)))
  object)
