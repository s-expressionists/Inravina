(in-package #:inravina/test)

(defmacro with-env ((stream &key right-margin) &body body)
  `(let ((*print-right-margin* ,right-margin))
     (with-output-to-string (,stream)
       ,@body)))


