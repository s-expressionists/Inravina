(in-package #:inravina)

(defvar *print-pprint-dispatch* nil)

(defgeneric copy-pprint-dispatch (client &optional table))

(defgeneric pprint-dispatch (client object &optional table))

(defgeneric pprint-fill (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-linear (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-tabular (client stream object &optional colon-p at-sign-p tabsize))

(defgeneric pprint-indent (client relative-to n &optional stream))

(defgeneric pprint-newline (client kind &optional stream))

(defgeneric pprint-tab (client kind colnum colinc &optional stream))

(defgeneric set-pprint-dispatch (client type-specifier function &optional priority table))


