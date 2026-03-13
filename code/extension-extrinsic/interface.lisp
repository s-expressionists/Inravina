(in-package #:inravina-extension-extrinsic)

(defclass client (incless-extension-extrinsic:client inravina-extension:client) ())

(defclass client-impl (client quaviver/schubfach:client) ())

(change-class incless-extension-extrinsic:*client* 'client-impl)

(inravina:define-interface :client-form incless-extension-extrinsic:*client*
                           :client-class client)
