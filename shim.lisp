(require :inravina)

(in-package :cl)

#+sbcl (setf sb-ext:*muffled-warnings* 'warning)

(fmakunbound 'pprint-dispatch)

(defun pprint-dispatch (object &optional table)
  (declare (ignore object table))
  (values nil nil))

(fmakunbound 'copy-pprint-dispatch)
(fmakunbound 'set-pprint-dispatch)

(declaim (ftype (function (&optional (or null inravina::dispatch-table))
                          inravina::dispatch-table)
                copy-pprint-dispatch)
         (ftype (function (t &optional (or null inravina::dispatch-table))
                          (values (or function symbol) boolean))
                pprint-dispatch)
         (ftype (function (t (or function symbol) &optional real inravina::dispatch-table)
                          null)
                set-pprint-dispatch)
         (type inravina::dispatch-table *print-pprint-dispatch*))

(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
  (inravina:copy-pprint-dispatch inravina:*client* table))

(defparameter *print-pprint-dispatch* (copy-pprint-dispatch nil))

(defun set-pprint-dispatch (type-specifier function &optional priority table)
  (inravina:set-pprint-dispatch inravina:*client* (or table *print-pprint-dispatch*) type-specifier function priority))

(defun pprint-fill (stream object &optional (colon-p t) at-sign-p)
  (inravina:pprint-fill inravina:*client* stream object colon-p at-sign-p))

(defun pprint-linear (stream object &optional (colon-p t) at-sign-p)
  (inravina:pprint-linear inravina:*client* stream object colon-p at-sign-p))

(defun pprint-tabular (stream object &optional (colon-p t) at-sign-p (tabsize 16))
  (inravina:pprint-tabular inravina:*client* stream object colon-p at-sign-p tabsize))

(defun pprint-indent (relative-to n &optional stream)
  (check-type relative-to-n (member :block :current))
  (inravina:pprint-indent inravina:*client* stream relative-to n))

(defun pprint-newline (kind &optional stream)
  (check-type kind (member :linear :fill :miser :mandatory))
  (inravina:pprint-newline inravina:*client* stream kind))

(defun pprint-tab (kind colnum colinc &optional stream)
  (check-type kind (member :line :section :line-relative :section-relative))
  (inravina:pprint-tab inravina:*client* stream kind colnum colinc))

(defun pprint-dispatch (object &optional table)
  (inravina:pprint-dispatch inravina:*client* (or table *print-pprint-dispatch*) object))

#+sbcl (setf sb-ext:*muffled-warnings* nil)    

