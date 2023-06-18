(in-package #:inravina-intrinsic)

(defclass intrinsic-client (incless-intrinsic:intrinsic-client)
  ())

(trivial-package-locks:with-unlocked-packages (:common-lisp)
  (inravina:define-interface intrinsic-client))

(setf incless-intrinsic:*client* *client*)
