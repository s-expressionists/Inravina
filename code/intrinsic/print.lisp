(in-package #:inravina-intrinsic)

(defclass intrinsic-client (incless-intrinsic:intrinsic-client)
  ())

(trivial-package-locks:with-unlocked-packages (:common-lisp)
  (inravina:define-interface (incless-intrinsic:*client* intrinsic-client)))

(setf incless-intrinsic:*client* (make-instance 'intrinsic-client))

(defmethod incless:write-object ((client intrinsic-client) object stream)
  (multiple-value-bind (func presentp)
      (and *print-pretty*
           (inravina:pprint-dispatch client *print-pprint-dispatch* object))
    (if presentp
        (funcall func stream object)
        (call-next-method)))
  object)

(defmethod inravina:execute-pprint-logical-block ((client intrinsic-client) stream object function
                                                  &key (prefix "")
                                                       (per-line-prefix "" per-line-prefix-p)
                                                       (suffix ""))
  (if per-line-prefix-p
      (inravina:pprint-logical-block (client stream object :per-line-prefix per-line-prefix :suffix suffix)
        (funcall function stream
                 (lambda () (inravina:pprint-exit-if-list-exhausted))
                 (lambda () (inravina:pprint-pop))))
      (inravina:pprint-logical-block (client stream object :prefix prefix :suffix suffix)
        (funcall function stream
                 (lambda () (inravina:pprint-exit-if-list-exhausted))
                 (lambda () (inravina:pprint-pop))))))
