(in-package #:inravina)

(defmethod text-width (client stream text &optional start end)
  (- (or end (length text))
     (or start 0)))

(defmethod break-position (client stream text)
  (1+ (or (position-if (lambda (ch)
                     (char/= ch #\Space))
                   text :from-end t)
      -1)))

(defmethod normalize-text (client stream text)
  text)

(defmethod arrange-text (client stream (text (eql nil)))
  (values nil 0 0))

(defmethod right-margin (client stream)
  (or *print-right-margin* 100))

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


