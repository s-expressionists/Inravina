(in-package #:inravina-native)

(defclass native-client ()
  ())

(defmethod inravina:copy-pprint-dispatch ((client native-client) table &optional read-only)
  (declare (ignorable read-only))
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
  (pprint-tabular stream object colon-p at-sign-p tabsize))

(defmethod inravina:pprint-indent ((client native-client) stream relative-to n)
  (pprint-indent relative-to n stream))

(defmethod inravina:pprint-newline ((client native-client) stream kind)
  (pprint-newline kind stream))

(defmethod inravina:pprint-tab ((client native-client) stream kind colnum colinc)
  (pprint-tab kind colnum colinc stream))

(defmethod inravina:execute-logical-block ((client native-client) stream object function
                                           &key (prefix "")
                                                per-line-prefix-p
                                                (suffix ""))
  (if per-line-prefix-p
      (pprint-logical-block (stream object :per-line-prefix prefix :suffix suffix)
        (funcall function stream
                 (lambda () (pprint-exit-if-list-exhausted))
                 (lambda () (pprint-pop))))
      (pprint-logical-block (stream object :prefix prefix :suffix suffix)
        (funcall function stream
                 (lambda () (pprint-exit-if-list-exhausted))
                 (lambda () (pprint-pop))))))

(defmethod inravina:pretty-stream-p ((client native-client) stream)
  (declare (ignorable stream))
  #+abcl (xp::xp-structure-p stream)
  #+ccl (or (ccl::xp-structure-p stream)
            (typep stream 'ccl::xp-stream))
  #+(or clasp ecl) (sys::pretty-stream-p stream)
  #+cmucl (pretty-print:pretty-stream-p stream)
  #+sbcl (sb-pretty:pretty-stream-p stream)
  #-(or ccl clasp cmucl ecl sbcl) nil)
