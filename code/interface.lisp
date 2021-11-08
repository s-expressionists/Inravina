(in-package #:inravina)

(defvar *options*
  `(:loop-current-indent-clauses (:as :for :with :initially :finally :do :doing)
    :loop-block-indent-clauses (:if :when :else :unless)
    :loop-top-level-clauses (:always :as :finally :for :initially :named :never
                             :repeat :thereis :until :while :with)
    :loop-compound-clauses (:do :doing :finally :initially)
    :loop-conditional-clauses (:if :when :unless :else)
    :loop-selectable-clauses (:append :appending :collect :collecting :count
                              :counting :do :doing :else :end :if :maximize :maximizing
                              :minimize :minimizing :nconc :nconcing :sum :summing
                              :unless :when)))

(defgeneric copy-pprint-dispatch (client table))

(defgeneric pprint-dispatch (client table object)
  (:method (client table object)
    (declare (ignore client table object))
    (values (lambda (stream object)
              (print-object object stream))
            nil)))

(defgeneric set-pprint-dispatch (client table type-specifier function priority))

(defgeneric write-object (client stream object))

(defgeneric pprint-fill (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-linear (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-tabular (client stream object &optional colon-p at-sign-p tabsize))

(defgeneric pprint-indent (client stream relative-to n)
  (:method (client stream relative-to n)
    (declare (ignore client stream relative-to n))))

(defgeneric pprint-newline (client stream kind)
  (:method (client stream kind)
    (declare (ignore client stream relative-to n))))

(defgeneric pprint-tab (client stream kind colnum colinc))

(defgeneric pprint-split (client stream text &optional start end))

(defgeneric pprint-text (client stream text &optional start end))

(defgeneric pprint-fill-plist (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-linear-plist (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-tabular-plist (client stream object &optional colon-p at-sign-p tabsize))

(defgeneric pprint-start-logical-block (client stream prefix per-line-prefix))

(defgeneric pprint-end-logical-block (client stream suffix))

(defgeneric make-pretty-stream (client stream))

(defgeneric pretty-stream-p (client stream)
  (:method (client stream)
    (declare (ignore client stream))
    nil))

(defgeneric text-width (client stream text &optional start end))

(defgeneric break-position (client stream text))

(defgeneric normalize-text (client stream text))

(defgeneric write-text (client stream line column text &optional start end))

(defgeneric miser-p (client stream))

(defgeneric right-margin (client stream))

(defgeneric pprint-block (client stream object))

(defgeneric pprint-defun (client stream object))

(defgeneric pprint-defmethod-with-qualifier (client stream object))

(defgeneric pprint-do (client stream object))

(defgeneric pprint-dolist (client stream object))

(defgeneric pprint-let (client stream object))

(defgeneric pprint-bindings (client stream object))

(defgeneric pprint-eval-when (client stream object))

(defgeneric pprint-progn (client stream object))

(defgeneric pprint-progv (client stream object))

(defgeneric pprint-tagbody (client stream object))

(defgeneric pprint-function-call (client stream object &optional argument-count))

(defgeneric pprint-argument-list (client stream object &optional argument-count))

(defgeneric pprint-with-hash-table-iterator (client stream object))

(defgeneric pprint-with-compilation-unit (client stream object))

(defgeneric pprint-pprint-logical-block (client stream object))

(defgeneric pprint-lambda-list (client stream object))

(defgeneric pprint-lambda (client stream object))

(defgeneric pprint-extended-loop (client stream object))

(defgeneric pprint-simple-loop (client stream object))

(defgeneric pprint-array (client stream object))

(defgeneric pprint-quote (client stream object))

(defgeneric pprint-function-quote (client stream object))

