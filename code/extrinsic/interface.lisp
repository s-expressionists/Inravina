(in-package #:inravina-extrinsic)

(defclass extrinsic-client (incless-extrinsic:extrinsic-client)
  ())

(defclass extrinsic-client-impl
    (extrinsic-client quaviver/schubfach:client)
  ())

(change-class incless-extrinsic:*client* 'extrinsic-client-impl)

(inravina:define-interface :client-form incless-extrinsic:*client*
                           :client-class extrinsic-client)
