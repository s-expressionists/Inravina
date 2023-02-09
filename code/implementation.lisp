(in-package #:inravina)

(defmethod break-position (client stream text)
  (declare (ignore client stream))
  (1+ (or (position-if (lambda (ch)
                         (char/= ch #\Space))
                       text :from-end t)
          -1)))

(defmethod normalize-text (client stream text)
  (declare (ignore client stream))
  text)
