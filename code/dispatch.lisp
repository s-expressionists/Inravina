(in-package #:inravina)

(defclass dispatch-entry ()
  ((type-specifier
     :accessor dispatch-entry-type-specifier
     :initarg :type-specifier)
   (test-function
     :accessor dispatch-entry-test-function
     :initarg :test-function)
   (function
     :accessor dispatch-entry-function
     :initarg :function)
   (priority
     :accessor dispatch-entry-priority
     :initarg :priority
     :initform 0
     :type real)))

(defclass dispatch-table ()
  ((entries
     :accessor dispatch-table-entries
     :initarg :entries
     :initform nil
     :type list)))

(defmethod print-object ((entry dispatch-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format stream "type-specifier=~S, priority=~S"
            (dispatch-entry-type-specifier entry)
            (dispatch-entry-priority entry))))

(defmethod print-object ((table dispatch-table) stream)
  (print-unreadable-object (table stream :type t :identity t)))

(defun simple-loop-form-p (form)
  (and (listp form)
       (eql (first form) 'loop)
       (cdr form)
       (not (symbolp (second form)))))

(deftype simple-loop-form ()
  `(satisfies simple-loop-form-p))

(defun extended-loop-form-p (form)
  (and (listp form)
       (eql (first form) 'loop)
       (cdr form)
       (symbolp (second form))))

(deftype extended-loop-form ()
  `(satisfies extended-loop-form-p))

(defun block-form-p (form)
  (and (listp form)
       (member (first form)
               '(block catch defconstant defparameter defvar
                 multiple-value-call multiple-value-prog1
                 print-unreadable-object prog1 return-from throw
                 unless unwind-protect when))))

(deftype block-form ()
  `(satisfies block-form-p))

(defun do-form-p (form)
  (and (listp form)
       (member (first form)
               '(do do*))))

(deftype do-form ()
  `(satisfies do-form-p))

(defun dolist-form-p (form)
  (and (listp form)
       (member (first form)
               '(dolist do-symbols do-external-symbols
                 do-all-symbols dotimes))))

(deftype dolist-form ()
  `(satisfies dolist-form-p))

(defun eval-when-form-p (form)
  (and (listp form)
       (member (first form)
               '(defstruct eval-when multiple-value-setq))))

(deftype eval-when-form ()
  `(satisfies eval-when-form-p))

(defun let-form-p (form)
  (and (listp form)
       (member (first form)
               '(let let*))))

(deftype let-form ()
  `(satisfies let-form-p))

(defun with-hash-table-iterator-form-p (form)
  (and (listp form)
       (member (first form)
               '(with-hash-table-iterator with-open-stream
                 with-package-iterator with-simple-restart))))

(deftype with-hash-table-iterator-form ()
  `(satisfies with-hash-table-iterator-form-p))

(defun with-compilation-unit-form-p (form)
  (and (listp form)
       (eql (first form) 'with-compilation-unit)))

(deftype with-compilation-unit-form ()
  `(satisfies with-compilation-unit-form-p))

(defun pprint-logical-block-form-p (form)
  (and (listp form)
       (member (first form)
               '(pprint-logical-block print-unreadable-object
                 with-input-from-string with-open-file
                 with-output-to-string))))

(deftype pprint-logical-block-form ()
  `(satisfies pprint-logical-block-form-p))

(defun defun-form-p (form)
  (and (listp form)
       (or (member (first form)
                   '(define-modify-macro define-setf-expander
                     defmacro defsetf deftype defun))
           (and (eql (first form) 'defmethod)
                (listp (third form))))))

(deftype defun-form ()
  `(satisfies defun-form-p))

(defun defmethod-with-qualifier-form-p (form)
  (and (listp form)
       (eql (first form) 'defmethod)
       (not (listp (third form)))))

(deftype defmethod-with-qualifier-form ()
  `(satisfies defmethod-with-qualifier-form-p))

(defun function-call-form-p (form)
  (and form
       (listp form)
       (symbolp (first form))))

(deftype function-call-form ()
  `(satisfies function-call-form-p))

(defun quote-form-p (form)
  (and (listp form)
       (eql (first form) 'quote)))

(deftype quote-form ()
  `(satisfies quote-form-p))

(defun function-quote-form-p (form)
  (and (listp form)
       (eql (first form) 'function)))

(deftype function-quote-form ()
  `(satisfies function-quote-form-p))

(defvar +default-dispatch-entries+
  '((block-form                    -10 pprint-block)
    (do-form                       -10 pprint-do)
    (dolist-form                   -10 pprint-dolist)
    (defun-form                    -10 pprint-defun)
    (defmethod-with-qualifier-form -10 pprint-defmethod-with-qualifier)
    (eval-when-form                -10 pprint-eval-when)
    (let-form                      -10 pprint-let)
    (with-hash-table-iterator-form -10 pprint-with-hash-table-iterator)
    (with-compilation-unit-form    -10 pprint-with-compilation-unit)
    (pprint-logical-block-form     -10 pprint-pprint-logical-block)
    (extended-loop-form            -10 pprint-extended-loop)
    (quote-form                    -10 pprint-quote)
    (function-quote-form           -10 pprint-function-quote)
    (simple-loop-form              -10 pprint-simple-loop)
    ((and array
          (not string)
          (not bit-vector))        -10 pprint-array)
    (function-call-form            -20 pprint-function-call)))

(defmethod copy-pprint-dispatch ((client client) (table (eql nil)))
  (let ((new-table (make-instance 'dispatch-table)))
    (loop for (type priority name . rest) in +default-dispatch-entries+
          do (set-pprint-dispatch client new-table
                                  type
                                  (lambda (stream object)
                                    (apply (fdefinition name) *client* stream object rest))
                                  priority))
    new-table))

(defmethod pprint-dispatch (client (table dispatch-table) object)
  (if (and (not *print-array*)
           (arrayp object))
      (values nil nil)
      (dolist (entry (dispatch-table-entries table) (values nil nil))
        (when (funcall (dispatch-entry-test-function entry) object)
          (return (values (dispatch-entry-function entry) t))))))

(defmethod set-pprint-dispatch (client (table dispatch-table) type-specifier (function (eql nil)) priority)
  (setf (dispatch-table-entries table)
        (delete type-specifier (dispatch-table-entries table) :test #'equal))
  nil)

(defmethod set-pprint-dispatch (client (table dispatch-table) type-specifier function priority)
  (let ((entry (find type-specifier (dispatch-table-entries table)
                     :test #'equal :key #'dispatch-entry-type-specifier))
        (wrapped-function (lambda (stream object &aux (*client* client))
                            (funcall function stream object)))
        (test-function (cond
                         ((or (not (listp type-specifier))
                              (not (equal 'cons (car type-specifier))))
                           (lambda (object)
                             (typep object type-specifier)))
                         ((third type-specifier)
                           (lambda (object)
                             (and (consp object)
                                  (typep (first object) (second type-specifier))
                                  (typep (second object) (third type-specifier)))))
                         (t
                           (lambda (object)
                             (and (consp object)
                                  (typep (first object) (second type-specifier))))))))
    (if entry
      (setf (dispatch-entry-function entry) function
            (dispatch-entry-test-function entry) test-function
            (dispatch-entry-priority entry) (or priority 0))
      (push (make-instance 'dispatch-entry
                           :type-specifier type-specifier
                           :test-function test-function
                           :function function
                           :priority (or priority 0))
            (dispatch-table-entries table)))
    (setf (dispatch-table-entries table)
          (stable-sort (dispatch-table-entries table) #'> :key #'dispatch-entry-priority)))
  nil)
