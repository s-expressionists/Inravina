(in-package #:inravina/intrinsic)

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
  (if *print-pretty*
      (inravina:pprint-fill inravina:*client* stream object colon-p at-sign-p)
      (print-object object stream)))

(defun pprint-linear (stream object &optional (colon-p t) at-sign-p)
  (if *print-pretty*
      (inravina:pprint-linear inravina:*client* stream object colon-p at-sign-p)
      (print-object object stream)))

(defun pprint-tabular (stream object &optional (colon-p t) at-sign-p (tabsize 16))
  (if *print-pretty*
      (inravina:pprint-tabular inravina:*client* stream object colon-p at-sign-p tabsize)
      (print-object object stream)))

(defun pprint-indent (relative-to n &optional stream)
  (check-type relative-to (member :block :current))
  (inravina:pprint-indent inravina:*client* stream relative-to n)
  nil)

(defun pprint-newline (kind &optional stream)
  (check-type kind (member :linear :fill :miser :mandatory))
  (inravina:pprint-newline inravina:*client* stream kind)
  nil)

(defun pprint-tab (kind colnum colinc &optional stream)
  (check-type kind (member :line :section :line-relative :section-relative))
  (inravina:pprint-tab inravina:*client* stream kind colnum colinc))

(defun pprint-dispatch (object &optional table)
  (inravina:pprint-dispatch inravina:*client* (or table *print-pprint-dispatch*) object))

(defun write-object (object stream)
  (if *print-pretty*
      (funcall (pprint-dispatch object) stream object)
      (print-object object stream))
  object)

(defun write
    (object
     &key (stream *standard-output*)
          ((:array *print-array*) *print-array*)
          ((:base *print-base*) *print-base*)
          ((:case *print-case*) *print-case*)
          ((:circle *print-circle*) *print-circle*)
          ((:escape *print-escape*) *print-escape*)
          ((:gensym *print-gensym*) *print-gensym*)
          ((:length *print-length*) *print-length*)
          ((:level *print-level*) *print-level*)
          ((:lines *print-lines*) *print-lines*)
          ((:miser-width *print-miser-width*) *print-miser-width*)
          ((:pprint-dispatch *print-pprint-dispatch*) *print-pprint-dispatch*)
          ((:pretty *print-pretty*) *print-pretty*)
          ((:radix *print-radix*) *print-radix*)
          ((:readably *print-readably*) *print-readably*)
          ((:right-margin *print-right-margin*) *print-right-margin*))
  (write-object object stream))

(defun write-to-string
    (object
     &key ((:array *print-array*) *print-array*)
          ((:base *print-base*) *print-base*)
          ((:case *print-case*) *print-case*)
          ((:circle *print-circle*) *print-circle*)
          ((:escape *print-escape*) *print-escape*)
          ((:gensym *print-gensym*) *print-gensym*)
          ((:length *print-length*) *print-length*)
          ((:level *print-level*) *print-level*)
          ((:lines *print-lines*) *print-lines*)
          ((:miser-width *print-miser-width*) *print-miser-width*)
          ((:pprint-dispatch *print-pprint-dispatch*) *print-pprint-dispatch*)
          ((:pretty *print-pretty*) *print-pretty*)
          ((:radix *print-radix*) *print-radix*)
          ((:readably *print-readably*) *print-readably*)
          ((:right-margin *print-right-margin*) *print-right-margin*))
  (with-output-to-string (stream)
    (write-object object stream)))

(defun prin1 (object &optional (stream *standard-output*))
  (let ((*print-escape* t))
    (write-object object stream)))

(defun princ (object &optional (stream *standard-output*))
  (let ((*print-escape* nil)
        (*print-readably* nil))
    (write-object object stream)))

(defun print (object &optional (stream *standard-output*))
  (write-char #\Newline stream)
  (prin1 object stream)
  (write-char #\Space stream)
  object)

(defun pprint (object &optional (stream *standard-output*))
  (write-char #\Newline stream)
  (let ((*print-pretty* t))
    (prin1 object stream)))

(defun prin1-to-string (object)
  (with-output-to-string (stream)
    (prin1 object stream)))

(defun princ-to-string (object)
  (with-output-to-string (stream)
    (princ object stream)))
