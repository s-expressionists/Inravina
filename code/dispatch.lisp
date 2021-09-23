(in-package #:inravina)

(defclass dispatch-entry ()
  ((type-specifier
     :accessor dispatch-entry-type-specifier
     :initarg :type-specifier)
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
            (pprint-dispatch-entry-type-specifier entry)
            (pprint-dispatch-entry-priority entry))))

(defmethod print-object ((table dispatch-table) stream)
  (print-unreadable-object (table stream :type t :identity t)))

(defmethod copy-pprint-dispatch ((client client) (table (eql nil)))
  (make-instance 'dispatch-table))

(defmethod pprint-dispatch (client object (table dispatch-table))
  (let ((entry (reduce (lambda (entry previous-entry)
                         (if (and (typep object (dispatch-entry-type-specifier entry))
                                  (or (not previous-entry)
                                      (< (dispatch-entry-priority previous-entry)
                                         (dispatch-entry-priority entry))))
                            entry
                            previous-entry))
                         (dispatch-table-entries table))))
    (if entry
      (values (dispatch-entry-function entry) t)
      (values nil nil))))

(defmethod set-pprint-dispatch (client type-specifier (function (eql nil)) priority (table dispatch-table))
  (setf (dispatch-table-entries table)
        (delete type-specifier (dispatch-table-entries table) :test #'equal))
  nil)

(defmethod set-pprint-dispatch (client type-specifier function priority (table dispatch-table))
  (let ((entry (find type-specifier (dispatch-table-entries table)
                     :test #'equal :key #'dispatch-entry-type-specifier))
        (wrapped-function (lambda (stream object &aux (*client* client))
                            (funcall function stream object))))
    (if entry
      (setf (dispatch-entry-function entry) wrapped-function
            (dispatch-entry-priority entry) (or priority 0))
      (push (make-instance 'dispatch-entry
                           :type-specifier type-specifier
                           :function wrapped-function
                           :priority (or priority 0))
            (dispatch-table-entries table))))
  nil)
