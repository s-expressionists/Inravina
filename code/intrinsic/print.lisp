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
    (inravina:pprint-fill *client* stream object colon-p at-sign-p)
    nil)

  (defun pprint-linear (stream object &optional (colon-p t) at-sign-p)
    (inravina:pprint-linear *client* stream object colon-p at-sign-p)
    nil)

  (defun pprint-tabular (stream object &optional (colon-p t) at-sign-p (tabsize 16))
    (inravina:pprint-tabular *client* stream object colon-p at-sign-p tabsize)
    nil)

  (defun pprint-indent (relative-to n &optional stream)
    (check-type relative-to (member :block :current))
    (when *print-pretty*
      (inravina:pprint-indent *client* (inravina:frob-output-stream stream) relative-to n))
    nil)

  (defun pprint-newline (kind &optional stream)
    (check-type kind (member :linear :fill :miser :mandatory))
    (when *print-pretty*
      (inravina:pprint-newline *client* (inravina:frob-output-stream stream) kind))
    nil)

  (defun pprint-tab (kind colnum colinc &optional stream)
    (check-type kind (member :line :section :line-relative :section-relative))
    (when *print-pretty*
      (inravina:pprint-tab *client* (inravina:frob-output-stream stream) kind colnum colinc))
    nil)

  (defun pprint-dispatch (object &optional (table *print-pprint-dispatch*))
    (check-type table (or null inravina::dispatch-table))
    (inravina:pprint-dispatch *client* table object))

  (defmacro pprint-logical-block ((stream-symbol object
                                   &key (prefix "" prefix-p)
                                     (per-line-prefix "" per-line-prefix-p)
                                     (suffix ""))
                                  &body body)
    (when (and prefix-p per-line-prefix-p)
      (error 'program-error))
    (check-type stream-symbol symbol)
    (let ((tag-name (gensym))
          (object-var (gensym))
          (count-var (gensym))
          (stream-var (cond ((null stream-symbol)
                             '*standard-output*)
                            ((eq t stream-symbol)
                             '*terminal-io*)
                            (t
                             stream-symbol))))
      `(inravina:do-pprint-logical-block *client* ,stream-symbol ,object
         ,(if per-line-prefix-p
              per-line-prefix
              prefix)
         ,per-line-prefix-p ,suffix
         (lambda (,stream-var ,object-var &aux (,count-var 0))
           (declare (ignorable ,stream-var ,object-var ,count-var))
           (block ,tag-name
             (macrolet ((pprint-exit-if-list-exhausted ()
                          '(unless ,object-var
                            (return-from ,tag-name)))
                        (pprint-pop ()
                          '(progn
                            (unless (inravina:pprint-pop-p *client* ,stream-var
                                                           ,object-var ,count-var)
                              (return-from ,tag-name))
                            (incf ,count-var)
                            (pop ,object-var))))
               ,@body))))))

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
