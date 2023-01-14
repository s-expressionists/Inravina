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

(defmethod pprint-split (client stream text &optional start end)
  (prog (pos)
   next
    (setf pos (position #\newline text :start (or start 0) :end end))
    (when pos
      (pprint-text client stream text start pos)
      (pprint-newline client stream :literal-mandatory)
      (setf start (1+ pos))
      (go next))
    (pprint-text client stream text start end)))

