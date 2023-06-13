(in-package #:inravina-shim)

(defclass shim-client (incless-native:native-client inravina-intrinsic:intrinsic-client)
  ())

(setf inravina-intrinsic:*client* (make-instance 'shim-client))
