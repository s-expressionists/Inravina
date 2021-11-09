(in-package #:inravina)

(defclass client (incless:standard-client)
  ())

(defvar *client* (make-instance 'client))


