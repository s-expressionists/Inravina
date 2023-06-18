(in-package #:inravina-shim)

(defclass shim-client (incless-native:native-client)
  ())

(trivial-package-locks:with-unlocked-packages (:common-lisp)
  #+sbcl (handler-bind ((warning (lambda (condition)
                                   (muffle-warning condition))))
           (declaim (type inravina::dispatch-table *print-pprint-dispatch*)))
  (inravina:define-interface shim-client t))

;;; The following hacks are all to make WITH-STANDARD-IO-SYNTAX work.

#+(or clasp ecl)
(setf (first (cdr si::+io-syntax-progv-list+)) *standard-pprint-dispatch*)

#+ecl
(setf (first (cdr si::+ecl-syntax-progv-list+)) *standard-pprint-dispatch*)

#+ccl
(setf ccl::*standard-pprint-dispatch-table* *standard-pprint-dispatch*)

#+sbcl
(setf sb-pretty::*standard-pprint-dispatch-table* *standard-pprint-dispatch*)

#+cmucl
(setf pp::*initial-pprint-dispatch* *standard-pprint-dispatch*)

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
