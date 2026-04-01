(in-package #:inravina-extension)

(defclass client (inravina:client) ())

(defmethod trinsic:features-list nconc ((client client))
  (list :pprint/inravina-extension))
