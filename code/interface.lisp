(in-package #:inravina)

;(defvar *print-pprint-dispatch* nil)

(defgeneric copy-pprint-dispatch (client table))

(defgeneric pprint-dispatch (client table object))

(defgeneric set-pprint-dispatch (client table type-specifier function priority))

(defgeneric pprint-fill (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-linear (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-tabular (client stream object &optional colon-p at-sign-p tabsize))

(defgeneric pprint-indent (client stream relative-to n))

(defgeneric pprint-newline (client stream kind))

(defgeneric pprint-tab (client stream kind colnum colinc))

(defgeneric pprint-split (client stream text &optional start end))

(defgeneric pprint-text (client stream text &optional start end))

(defgeneric pprint-fill-plist (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-linear-plist (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-tabular-plist (client stream object &optional colon-p at-sign-p tabsize))

(defgeneric pprint-start-logical-block (client stream prefix per-line-prefix))

(defgeneric pprint-end-logical-block (client stream suffix))

(defgeneric make-pretty-stream (client stream))

(defgeneric text-width (client stream text &optional start end))

(defgeneric break-position (client stream text))

(defgeneric normalize-text (client stream text))

(defgeneric write-text (client stream line column text &optional start end))

(defgeneric miser-p (client stream))

(defgeneric right-margin (client stream))

(defgeneric pprint-block (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-defun (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-do (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-dolist (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-let (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-bindings (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-eval-when (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-progn (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-progv (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-tagbody (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-function-call (client stream object &optional colon-p at-sign-p argument-count))

(defgeneric pprint-argument-list (client stream object &optional colon-p at-sign-p argument-count))

(defgeneric pprint-with-hash-table-iterator (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-with-compilation-unit (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-pprint-logical-block (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-lambda-list (client stream object &optional colon-p at-sign-p))

