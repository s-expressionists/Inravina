(in-package #:inravina/intrinsic)

(trivial-package-locks:with-unlocked-packages (:common-lisp)

#+sbcl (declaim (type inravina::dispatch-table *print-pprint-dispatch*))

(defparameter *print-pprint-dispatch* inravina:*print-pprint-dispatch*)

(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
  (inravina:copy-pprint-dispatch inravina:*client* table))

(defun set-pprint-dispatch (type-specifier function &optional priority table)
  (check-type priority real)
  (inravina:set-pprint-dispatch inravina:*client* (or table *print-pprint-dispatch*) type-specifier function priority))

(defun pprint-fill (stream object &optional (colon-p t) at-sign-p)
  (inravina:pprint-fill inravina:*client* stream object colon-p at-sign-p)
  nil)

(defun pprint-linear (stream object &optional (colon-p t) at-sign-p)
  (inravina:pprint-linear inravina:*client* stream object colon-p at-sign-p)
  nil)

(defun pprint-tabular (stream object &optional (colon-p t) at-sign-p (tabsize 16))
  (inravina:pprint-tabular inravina:*client* stream object colon-p at-sign-p tabsize)
  nil)

(defun pprint-indent (relative-to n &optional stream)
  (check-type relative-to (member :block :current))
  (when *print-pretty*
    (inravina:pprint-indent inravina:*client* stream relative-to n))
  nil)

(defun pprint-newline (kind &optional stream)
  (check-type kind (member :linear :fill :miser :mandatory))
  (when *print-pretty*
    (inravina:pprint-newline inravina:*client* stream kind))
  nil)

(defun pprint-tab (kind colnum colinc &optional stream)
  (check-type kind (member :line :section :line-relative :section-relative))
  (when *print-pretty*
    (inravina:pprint-tab inravina:*client* stream kind colnum colinc))
  nil)

(defun pprint-dispatch (object &optional table)
  (inravina:pprint-dispatch inravina:*client* (or table *print-pprint-dispatch*) object))

(defmacro pprint-logical-block ((stream-symbol object
                                &key (prefix nil prefix-p)
                                     (per-line-prefix nil per-line-prefix-p)
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
    `(inravina:do-pprint-logical-block inravina:*client* ,stream-symbol ,object
                              ,prefix ,per-line-prefix ,suffix
                              (lambda (,stream-var ,object-var &aux (,count-var 0))
                                (declare (ignorable ,stream-var ,object-var))
                                (block ,tag-name
                                  (macrolet ((pprint-exit-if-list-exhausted ()
                                               '(unless ,object-var
                                                  (return-from ,tag-name)))
                                             (pprint-pop ()
                                               '(progn
                                                  (unless (inravina:pprint-pop-p inravina:*client* ,stream-var
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
