(in-package #:inravina-intrinsic)

(defclass intrinsic-client (incless-intrinsic:intrinsic-client)
  ())

(defclass intrinsic-client-impl
    (intrinsic-client quaviver/schubfach:client)
  ())

(change-class incless-intrinsic:*client* 'intrinsic-client-impl)

(inravina:define-interface :client-form incless-intrinsic:*client*
                           :client-class intrinsic-client
                           :intrinsic t)
