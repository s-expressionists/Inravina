(in-package #:inravina-intrinsic)

(defclass intrinsic-client () ())

(defparameter *client* (make-instance 'intrinsic-client))

(defmethod inravina:make-dispatch-function ((client intrinsic-client) (pattern (eql :client-stream-object)) function rest)
  (lambda (stream object)
    (apply function *client* (inravina:make-pretty-stream *client* stream) object rest)))

(defmethod inravina:make-dispatch-function ((client intrinsic-client) (pattern (eql :client-object-stream)) function rest)
  (lambda (stream object)
    (apply function *client* object (inravina:make-pretty-stream *client* stream) rest)))

(defmethod inravina:make-dispatch-function ((client intrinsic-client) (pattern (eql :stream-object)) function rest)
  (lambda (stream object)
    (apply function (inravina:make-pretty-stream *client* stream) object rest)))

(defmethod inravina:make-dispatch-function ((client intrinsic-client) (pattern (eql :object-stream)) function rest)
  (lambda (stream object)
    (apply function object (inravina:make-pretty-stream *client* stream) rest)))

(defparameter *initial-pprint-dispatch* (inravina:copy-pprint-dispatch *client* nil t))

(defparameter *standard-pprint-dispatch* (inravina:copy-pprint-dispatch *client* nil t))

(trivial-package-locks:with-unlocked-packages (:common-lisp)

  #+sbcl (declaim (type inravina::dispatch-table *print-pprint-dispatch*))

  (defparameter *print-pprint-dispatch* (inravina:copy-pprint-dispatch *client* t))

  (defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
    (check-type table (or null inravina::dispatch-table))
    (inravina:copy-pprint-dispatch *client* table))

  (defun set-pprint-dispatch (type-specifier function &optional (priority 0) (table *print-pprint-dispatch*))
    (check-type priority real)
    (check-type table inravina::dispatch-table)
    (inravina:set-pprint-dispatch *client* table type-specifier function priority))

  (defun pprint-fill (stream object &optional (colon-p t) at-sign-p)
    (inravina:pprint-fill *client* (inravina:coerce-output-stream-designator stream)
                          object colon-p at-sign-p)
    nil)

  (defun pprint-linear (stream object &optional (colon-p t) at-sign-p)
    (inravina:pprint-linear *client* (inravina:coerce-output-stream-designator stream)
                            object colon-p at-sign-p)
    nil)

  (defun pprint-tabular (stream object &optional (colon-p t) at-sign-p (tabsize 16))
    (inravina:pprint-tabular *client* (inravina:coerce-output-stream-designator stream)
                             object colon-p at-sign-p tabsize)
    nil)

  (defun pprint-indent (relative-to n &optional stream)
    (check-type relative-to (member :block :current))
    (inravina:pprint-indent *client* (inravina:coerce-output-stream-designator stream)
                            relative-to n)
    nil)

  (defun pprint-newline (kind &optional stream)
    (check-type kind (member :linear :fill :miser :mandatory))
    (inravina:pprint-newline *client* (inravina:coerce-output-stream-designator stream)
                             kind)
    nil)

  (defun pprint-tab (kind colnum colinc &optional stream)
    (check-type kind (member :line :section :line-relative :section-relative))
    (inravina:pprint-tab *client* (inravina:coerce-output-stream-designator stream)
                         kind colnum colinc)
    nil)

  (defun pprint-dispatch (object &optional (table *print-pprint-dispatch*))
    (check-type table (or null inravina::dispatch-table))
    (inravina:pprint-dispatch *client* (or table *initial-pprint-dispatch*) object))

  (defmacro pprint-logical-block ((stream-symbol object
                                   &key (prefix "" prefix-p)
                                     (per-line-prefix "" per-line-prefix-p)
                                     (suffix "" suffix-p))
                                  &body body)
    (inravina:expand-logical-block '*client* stream-symbol object
                                   prefix prefix-p per-line-prefix per-line-prefix-p suffix suffix-p
                                   'pprint-exit-if-list-exhausted 'pprint-pop
                                   body))

  (defmacro pprint-exit-if-list-exhausted ()
    "Tests whether or not the list passed to the lexically current logical block has
   been exhausted. If this list has been reduced to nil, pprint-exit-if-list-exhausted
   terminates the execution of the lexically current logical block except for the
   printing of the suffix. Otherwise pprint-exit-if-list-exhausted returns nil."
    (error "PPRINT-EXIT-IF-LIST-EXHAUSTED must be lexically inside PPRINT-LOGICAL-BLOCK."))

  (defmacro pprint-pop ()
    "Pops one element from the list being printed in the lexically current logical
   block, obeying *print-length* and *print-circle*."
    (error "PPRINT-POP must be lexically inside PPRINT-LOGICAL-BLOCK.")))
