(in-package #:inravina-extrinsic)

(defclass client (incless-extrinsic:client inravina:client) ())

(defclass client-impl (client quaviver/schubfach:client) ())

(change-class incless-extrinsic:*client* 'client-impl)

(inravina:define-interface :client-form incless-extrinsic:*client*
                           :client-class client)
