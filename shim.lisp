(setf *print-pretty* nil)

(asdf:load-system :incless-native)
(asdf:load-system :inravina-intrinsic)

(defclass printer-client (incless-native:native-client inravina-intrinsic:intrinsic-client)
  ())

(setf inravina-intrinsic:*client* (make-instance 'printer-client))

(setf *print-pretty* t)
