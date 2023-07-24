(in-package #:inravina-native)

(defclass native-client ()
  ())

(defmethod inravina:copy-pprint-dispatch ((client native-client) table &optional read-only)
  (declare (ignorable readonly))
  (copy-pprint-dispatch table))

(defmethod inravina:pprint-dispatch ((client native-client) table object)
  (pprint-dispatch table object))

(defmethod inravina:set-pprint-dispatch ((client native-client) table type-specifier function &optional priority pattern arguments)
  (declare (ignore pattern arguments))
  (set-pprint-dispatch type-specifier function priority table))

(defmethod inravina:pprint-fill ((client native-client) stream object &optional colon-p at-sign-p)
  (pprint-fill stream object colon-p at-sign-p))

(defmethod inravina:pprint-linear ((client native-client) stream object &optional colon-p at-sign-p)
  (pprint-linear stream object colon-p at-sign-p))

(defmethod inravina:pprint-tabular ((client native-client) stream object &optional colon-p at-sign-p tabsize)
  (pprint-tabular stream object colon-p at-sign-p))

(defmethod inravina:pprint-indent ((client native-client) stream relative-to n)
  (pprint-indent relative-to n stream))

(defmethod inravina:pprint-newline ((client native-client) stream kind)
  (pprint-newline kind stream))

(defmethod inravina:pprint-tab ((client native-client) stream kind colnum colinc)
  (pprint-tab kind colnum colinc stream))

(defmethod inravina:execute-pprint-logical-block ((client native-client) stream object function
                                                  &key (prefix "")
                                                       (per-line-prefix "" per-line-prefix-p)
                                                       (suffix ""))
  (if per-line-prefix-p
      (pprint-logical-block (stream object :per-line-prefix per-line-prefix :suffix suffix)
        (funcall function stream
                 (lambda () (pprint-exit-if-list-exhausted))
                 (lambda () (pprint-pop))))
      (pprint-logical-block (stream object :prefix prefix :suffix suffix)
        (funcall function stream
                 (lambda () (pprint-exit-if-list-exhausted))
                 (lambda () (pprint-pop))))))