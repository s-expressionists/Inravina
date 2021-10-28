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

(deftype block-form ()
  `(member block catch defconstant defparameter defvar
           multiple-value-call multiple-value-prog1
           print-unreadable-object prog1 return-from throw
           unless unwind-protect when))

(deftype do-form ()
  `(member do do*))

(deftype dolist-form ()
  `(member dolist do-symbols do-external-symbols
           do-all-symbols dotimes))

(deftype eval-when-form ()
  `(member defstruct eval-when multiple-value-setq))

(deftype let-form ()
  `(member let let*))

(deftype with-hash-table-iterator-form () ; No keys
  `(member with-hash-table-iterator with-open-stream
           with-package-iterator with-simple-restart))

(deftype with-compilation-unit-form () ; Zero argument
  `(member with-compilation-unit))

(deftype pprint-logical-block-form () ; Two arguments
  `(member pprint-logical-block print-unreadable-object
           with-input-from-string with-open-file
           with-output-to-string))

(deftype defun-form ()
  `(member define-modify-macro define-setf-expander
           defmacro defsetf deftype defun))

(deftype defmethod-form ()
  `(member defmethod))

(defmethod copy-pprint-dispatch ((client client) (table (eql nil)))
  (let ((new-table (make-instance 'dispatch-table)))
    (set-pprint-dispatch client new-table
                         '(cons block-form)
                         (lambda (stream object)
                           (pprint-block client stream object t))
                         +default-dispatch-priority+)
    (set-pprint-dispatch client new-table
                         '(cons do-form)
                         (lambda (stream object)
                           (pprint-do client stream object t))
                         +default-dispatch-priority+)
    (set-pprint-dispatch client new-table
                         '(cons dolist-form)
                         (lambda (stream object)
                           (pprint-dolist client stream object t))
                         +default-dispatch-priority+)
    (set-pprint-dispatch client new-table
                         '(cons eval-when-form)
                         (lambda (stream object)
                           (pprint-eval-when client stream object t))
                         +default-dispatch-priority+)
    (set-pprint-dispatch client new-table
                         '(cons let-form)
                         (lambda (stream object)
                           (pprint-let client stream object t))
                         +default-dispatch-priority+)
    (set-pprint-dispatch client new-table
                         '(cons with-hash-table-iterator-form)
                         (lambda (stream object)
                           (pprint-with-hash-table-iterator client stream object t))
                         +default-dispatch-priority+)
    (set-pprint-dispatch client new-table
                         '(cons with-compilation-unit-form)
                         (lambda (stream object)
                           (pprint-with-compilation-unit client stream object t))
                         +default-dispatch-priority+)
    (set-pprint-dispatch client new-table
                         '(cons pprint-logical-block-form)
                         (lambda (stream object)
                           (pprint-pprint-logical-block client stream object t))
                         +default-dispatch-priority+)
    (set-pprint-dispatch client new-table
                         '(cons defun-form)
                         (lambda (stream object)
                           (pprint-defun client stream object t nil))
                         +default-dispatch-priority+)
    (set-pprint-dispatch client new-table
                         '(cons defmethod-form)
                         (lambda (stream object)
                           (pprint-defun client stream object t t))
                         +default-dispatch-priority+)
    (set-pprint-dispatch client new-table
                         '(cons symbol)
                         (lambda (stream object)
                           (pprint-function-call client stream object t))
                         (1- +default-dispatch-priority+))
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
