(in-package #:inravina-extrinsic)

(defclass extrinsic-client (incless-extrinsic:extrinsic-client)
  ())

(defclass extrinsic-client-impl
    (extrinsic-client quaviver/schubfach:client)
  ())

(inravina:define-interface (incless-extrinsic:*client* extrinsic-client))

(setf incless-extrinsic:*client* (make-instance 'extrinsic-client-impl))

(initialize-inravina)

(defmethod incless:write-object ((client extrinsic-client) object stream)
  (multiple-value-bind (func presentp)
      (and *print-pretty*
           (inravina:pprint-dispatch client *print-pprint-dispatch* object))
    (if presentp
        (funcall func stream object)
        (call-next-method)))
  object)

(defmacro with-standard-io-syntax (&body body)
  `(let ((*package* (find-package "CL-USER"))
         (*print-array* t)
         (*print-base* 10)
         (*print-case* :upcase)
         (*print-circle* nil)
         (*print-escape* t)
         (*print-gensym* t)
         (*print-length* nil)
         (*print-level* nil)
         (*print-lines* nil)
         (*print-miser-width* nil)
         (*print-pprint-dispatch* *standard-pprint-dispatch*)
         (*print-pretty* nil)
         (*print-radix* nil)
         (*print-readably* t)
         (*print-right-margin* nil)
         (*read-base* 10)
         (*read-default-float-format* 'single-float)
         (*read-eval* t)
         (*read-suppress* nil)
         (*readtable* (copy-readtable nil)))
     ,@body))
