(in-package #:inravina-shim)

(defclass shim-client (incless-native:native-client inravina-intrinsic:intrinsic-client)
  ())

(setf inravina-intrinsic:*client* (make-instance 'shim-client))

;;; The following hacks are all to make WITH-STANDARD-IO-SYNTAX work.

#+(or clasp ecl)
(setf (first (cdr si::+io-syntax-progv-list+)) inravina-intrinsic:*standard-pprint-dispatch*)

#+ecl
(setf (first (cdr si::+ecl-syntax-progv-list+)) inravina-intrinsic:*standard-pprint-dispatch*)

#+ccl
(setf ccl::*standard-pprint-dispatch-table* inravina-intrinsic:*standard-pprint-dispatch*)

#+sbcl
(setf sb-pretty::*standard-pprint-dispatch-table* inravina-intrinsic:*standard-pprint-dispatch*)

#+cmucl
(setf pp::*initial-pprint-dispatch* inravina-intrinsic:*standard-pprint-dispatch*)
