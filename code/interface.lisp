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

(defgeneric pprint-split (client stream text &optional start end))

(defgeneric pprint-text (client stream text &optional start end))

(defgeneric pprint-start-logical-block (client stream prefix per-line-prefix))

(defgeneric pprint-end-logical-block (client stream suffix))

(defgeneric make-pretty-stream (client stream))

(defgeneric text-width (client stream text &optional start end))

(defgeneric break-position (client stream text))

(defgeneric normalize-text (client stream text))

(defgeneric write-text (client stream line column text))

(defgeneric miser-p (client stream))

(defgeneric right-margin (client stream))

(defgeneric column (client stream))

(defgeneric (setf column) (new-value client stream))

(defgeneric line (client stream))

(defgeneric (setf line) (new-value client stream))
