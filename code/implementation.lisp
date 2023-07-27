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

(defmethod execute-logical-block (client stream object function
                                  &key (prefix "")
                                       per-line-prefix-p
                                       (suffix ""))
  (if per-line-prefix-p
      (pprint-logical-block (client stream object :per-line-prefix prefix :suffix suffix)
        (funcall function stream
                 (lambda () (pprint-exit-if-list-exhausted))
                 (lambda () (pprint-pop))))
      (pprint-logical-block (client stream object :prefix prefix :suffix suffix)
        (funcall function stream
                 (lambda () (pprint-exit-if-list-exhausted))
                 (lambda () (pprint-pop))))))
