(in-package #:inravina-extension-intrinsic)

(defclass intrinsic-client (incless-extension-intrinsic:intrinsic-client)
  ())

(defclass intrinsic-client-impl
    (intrinsic-client quaviver/schubfach:client)
  ())

(change-class incless-extension-intrinsic:*client* 'intrinsic-client-impl)

(inravina:define-interface :client-form incless-extension-intrinsic:*client*
                           :client-class intrinsic-client
                           :intrinsic t)

