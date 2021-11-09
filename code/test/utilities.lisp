(in-package #:inravina/test)

(defmacro with-env ((stream &key right-margin) &body body)
  `(let ((*print-right-margin* ,right-margin))
     (with-output-to-string (,stream)
       ,@body)))

(setf incless:*client* inravina:*client*)

(defmethod incless:print-object-using-client (client (object symbol) stream)
  (declare (ignore client))
  (print-object object stream))
