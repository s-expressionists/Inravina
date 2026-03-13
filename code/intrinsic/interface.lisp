(in-package #:inravina-intrinsic)

(defclass client (incless-intrinsic:client inravina:client) ())

(defclass client-impl (client quaviver/schubfach:client) ())

(change-class incless-intrinsic:*client* 'client-impl)

(inravina:define-interface :client-form incless-intrinsic:*client*
                           :client-class client
                           :intrinsic t)
