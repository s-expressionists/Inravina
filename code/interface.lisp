(in-package #:inravina)

;(defvar *print-pprint-dispatch* nil)

(defgeneric copy-pprint-dispatch (client table))

(defgeneric pprint-dispatch (client object table))

(defgeneric set-pprint-dispatch (client type-specifier function priority table))

(defgeneric pprint-fill (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-linear (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-tabular (client stream object &optional colon-p at-sign-p tabsize))

(defgeneric pprint-indent (client relative-to n stream))

(defgeneric pprint-newline (client kind stream))

(defgeneric pprint-tab (client kind colnum colinc stream))

(defgeneric make-pretty-stream (client stream))

(defgeneric pprint-start-logical-block (client stream prefix per-line-prefix suffix))

(defgeneric pprint-end-logical-block (client stream))

(defgeneric text-width (client stream text))

(defgeneric advance-to-column (client stream column))

(defgeneric miser-p (client stream))

(defgeneric right-margin (client stream))

(defgeneric column (client stream))

(defgeneric line (client stream))
