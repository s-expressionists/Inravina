(in-package #:inravina)

(defmacro pprint-format-logical-block ((client stream object &key paren) &body body)
  `(pprint-logical-block (,client ,stream ,object :prefix (if ,paren "(" "")
                                                  :suffix (if ,paren ")" ""))
     ,@body))

(defmacro pprint-list ((client stream object &key paren tabsize newline) &body body)
  `(pprint-format-logical-block (,client ,stream ,object :paren ,paren)
     ,@body
    (pprint-exit-if-list-exhausted)
    (loop do (write-object ,client ,stream (pprint-pop))
             (pprint-exit-if-list-exhausted)
             (write-char #\Space ,stream)
             ,@(when tabsize
                 `((pprint-tab ,client ,stream :section-relative 0 ,tabsize)))
             ,@(when newline
                 `((pprint-newline ,client ,stream ,newline))))))

(defmacro pprint-plist ((client stream object &key paren tabsize newline) &body body)
  `(pprint-format-logical-block (,client ,stream ,object :paren ,paren)
     ,@body
    (pprint-exit-if-list-exhausted)
    (loop do (write-object ,client ,stream (pprint-pop))
             (pprint-exit-if-list-exhausted)
             (write-char #\Space ,stream)
             ,@(when tabsize
                 `((pprint-tab ,client ,stream :section-relative 0 ,tabsize)))
             (write-object ,client ,stream (pprint-pop))
             (pprint-exit-if-list-exhausted)
             (write-char #\Space ,stream)
             ,@(when tabsize
                 `((pprint-tab ,client ,stream :section-relative 0 ,tabsize)))
             ,@(when newline
                 `((pprint-newline ,client ,stream ,newline))))))

(defmethod pprint-fill (client stream object &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (pprint-list (client stream object :paren colon-p :newline :fill)))

(defmethod pprint-linear (client stream object &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (pprint-list (client stream object :paren colon-p :newline :linear)))

(defmethod pprint-tabular (client stream object &optional colon-p at-sign-p tabsize)
  (declare (ignore at-sign-p))
  (unless tabsize
    (setq tabsize 16))
  (pprint-list (client stream object :paren colon-p :newline :fill :tabsize tabsize)))

(defmethod pprint-fill-plist (client stream object &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (pprint-plist (client stream object :paren colon-p :newline :fill)))

(defmethod pprint-linear-plist (client stream object &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (pprint-plist (client stream object :paren colon-p :newline :linear)))

(defmethod pprint-tabular-plist (client stream object &optional colon-p at-sign-p tabsize)
  (declare (ignore at-sign-p))
  (unless tabsize
    (setq tabsize 16))
  (pprint-plist (client stream object :paren colon-p :newline :fill :tabsize tabsize)))
  
