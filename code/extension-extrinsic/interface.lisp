(in-package #:inravina-extension-extrinsic)

(defclass extrinsic-client (incless-extension-extrinsic:extrinsic-client)
  ())

(defclass extrinsic-client-impl
    (extrinsic-client quaviver/schubfach:client)
  ())

(change-class incless-extension-extrinsic:*client* 'extrinsic-client-impl)

(inravina:define-interface :client-form incless-extension-extrinsic:*client*
                           :client-class extrinsic-client)
