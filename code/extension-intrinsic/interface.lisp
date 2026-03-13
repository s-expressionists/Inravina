(in-package #:inravina-extension-intrinsic)

(defclass client
    (incless-extension-intrinsic:client inravina-extension:client)
  ())

(defclass client-impl
    (client quaviver/schubfach:client)
  ())

(change-class incless-extension-intrinsic:*client* 'client-impl)

(inravina:define-interface :client-form incless-extension-intrinsic:*client*
                           :client-class client
                           :intrinsic t)

