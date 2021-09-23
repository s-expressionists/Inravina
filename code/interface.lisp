(in-package #:inravina)

(defvar *print-pprint-dispatch* nil)

(defgeneric copy-pprint-dispatch (client table))

(defgeneric pprint-dispatch (client object table))

(defgeneric set-pprint-dispatch (client type-specifier function priority table))

(defgeneric pprint-fill (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-linear (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-tabular (client stream object &optional colon-p at-sign-p tabsize))

(defgeneric pprint-indent (client relative-to n &optional stream))

(defgeneric pprint-newline (client kind &optional stream))

(defgeneric pprint-tab (client kind colnum colinc &optional stream))

(defgeneric make-pretty-stream (client stream))

(defgeneric pprint-start-logical-block (client stream prefix per-line-prefix suffix))

(defgeneric pprint-end-logical-block (client stream))
