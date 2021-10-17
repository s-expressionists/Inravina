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

(deftype block-form ()
  `(member block catch defconstant defparameter defvar
           multiple-value-call multiple-value-prog1
           print-unreadable-object prog1 return-from throw
           unless unwind-protect when))

(deftype eval-when-form ()
  `(member defstruct eval-when multiple-value-setq))

(deftype body-form-1-lambda-list ()
  `(member with-compilation-unit cl:pprint-logical-block
           print-unreadable-object with-hash-table-iterator
           with-input-from-string with-open-file
           with-open-stream with-output-to-string
           with-package-iterator with-simple-restart))

(defmethod copy-pprint-dispatch ((client client) (table (eql nil)))
  (let ((new-table (make-instance 'dispatch-table)))
    (set-pprint-dispatch client
                         '(cons block-form)
                         (lambda (stream object)
                           (pprint-block client stream object t))
                         -1 new-table)
    (set-pprint-dispatch client
                         '(cons eval-when-form)
                         (lambda (stream object)
                           (pprint-eval-when client stream object t))
                         -1 new-table)
    new-table))

(defmethod pprint-dispatch (client object (table dispatch-table))
  (dolist (entry (dispatch-table-entries table) (values nil nil))
    (when (funcall (dispatch-entry-test-function entry) object)
      (return (values (dispatch-entry-function entry) t)))))

(defmethod set-pprint-dispatch (client type-specifier (function (eql nil)) priority (table dispatch-table))
  (setf (dispatch-table-entries table)
        (delete type-specifier (dispatch-table-entries table) :test #'equal))
  nil)

(defmethod set-pprint-dispatch (client type-specifier function priority (table dispatch-table))
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
