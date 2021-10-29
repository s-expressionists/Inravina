(in-package #:inravina)

(defconstant +default-dispatch-priority+ -1)

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
       (not (symbolp (second form)))))

(deftype simple-loop-form ()
  `(satisfies simple-loop-form-p))

(defun extended-loop-form-p (form)
  (and (listp form)
       (eql (first form) 'loop)
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

(defvar +default-dispatch-entries+
  '((block-form                    pprint-block                    -1)
    (do-form                       pprint-do                       -1)
    (dolist-form                   pprint-dolist                   -1)
    (defun-form                    pprint-defun                    -1)
    (defmethod-with-qualifier-form pprint-defmethod-with-qualifier -1)
    (eval-when-form                pprint-eval-when                -1)
    (let-form                      pprint-let                      -1)
    (with-hash-table-iterator-form pprint-with-hash-table-iterator -1)
    (with-compilation-unit-form    pprint-with-compilation-unit    -1)
    (pprint-logical-block-form     pprint-pprint-logical-block     -1)
    (extended-loop-form            pprint-extended-loop            -1)
    (function-call-form            pprint-function-call            -2)))

(defmethod copy-pprint-dispatch ((client client) (table (eql nil)))
  (let ((new-table (make-instance 'dispatch-table)))
    (loop for (type name priority) in +default-dispatch-entries+
          do (set-pprint-dispatch client new-table
                                  type
                                  (lambda (stream object)
                                    (funcall (fdefinition name) client stream object))
                                  priority))
    new-table))

(defmethod pprint-dispatch (client (table dispatch-table) object)
  (dolist (entry (dispatch-table-entries table) (values nil nil))
    (when (funcall (dispatch-entry-test-function entry) object)
      (return (values (dispatch-entry-function entry) t)))))

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
                                  (typep (car object) (second type-specifier))
                                  (typep (car object) (third type-specifier)))))
                         (t
                           (lambda (object)
                             (and (consp object)
                                  (typep (car object) (second type-specifier))))))))
    (if entry
      (setf (dispatch-entry-function entry) wrapped-function
            (dispatch-entry-test-function entry) test-function
            (dispatch-entry-priority entry) (or priority 0))
      (push (make-instance 'dispatch-entry
                           :type-specifier type-specifier
                           :test-function test-function
                           :function wrapped-function
                           :priority (or priority 0))
            (dispatch-table-entries table)))
    (setf (dispatch-table-entries table)
          (sort (dispatch-table-entries table) #'> :key #'dispatch-entry-priority)))
  nil)
