(in-package #:inravina)

(defclass dispatch-entry ()
  ((type-specifier :accessor dispatch-entry-type-specifier
                   :initarg :type-specifier)
   (test-function :accessor dispatch-entry-test-function
                  :initarg :test-function)
   (function :accessor dispatch-entry-function
             :initarg :function)
   (priority :accessor dispatch-entry-priority
             :initarg :priority
             :initform 0
             :type real)))

(defun make-test-function (type-specifier)
  (cond ((or (not (listp type-specifier))
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
                (typep (first object) (second type-specifier)))))))

(defmethod initialize-instance :after ((instance dispatch-entry) &rest initargs &key)
  (declare (ignore initargs))
  (setf (dispatch-entry-test-function instance)
        (make-test-function (dispatch-entry-type-specifier instance))))

(defclass dispatch-table ()
  ((entries :accessor dispatch-table-entries
            :initarg :entries
            :initform nil
            :type list)
   (read-only :accessor dispatch-table-read-only-p
              :initarg :read-only
              :initform nil
              :type boolean)))

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

(defun pprint-logical-block-form-p (form)
  (and (listp form)
       (member (first form)
               '(cl:pprint-logical-block print-unreadable-object
                 with-input-from-string with-open-file
                 with-output-to-string))))

(deftype pprint-logical-block-form ()
  `(satisfies pprint-logical-block-form-p))

(defun pprint-logical-block-form-p/2 (form)
  (and (listp form)
       (member (first form)
               '(pprint-logical-block))))

(deftype pprint-logical-block-form/2 ()
  `(satisfies pprint-logical-block-form-p/2))

(defun defmethod-with-qualifier-form-p (form)
  (and (listp form)
       (eql (first form) 'defmethod)
       (not (listp (third form)))))

(deftype defmethod-with-qualifier-form ()
  `(satisfies defmethod-with-qualifier-form-p))

(defun call-form-p (form)
  (and form
       (listp form)
       (symbolp (first form))
       (fboundp (first form))))

(deftype call-form ()
  `(satisfies call-form-p))

(defun quote-form-p (form)
  (and (listp form)
       (cdr form)
       (listp (cdr form))
       (null (cddr form))
       (eql (first form) 'quote)))

(deftype quote-form ()
  `(satisfies quote-form-p))

#+(or (and clasp (not staging)) ecl sbcl)
(defun quasiquote-form-p (form)
  (and (listp form)
       (cdr form)
       (listp (cdr form))
       (null (cddr form))
       (eql (first form)
            #+clasp 'eclector.reader:quasiquote
            #+ecl 'si:quasiquote
            #+sbcl 'sb-int:quasiquote)))

#+(or (and clasp (not staging)) ecl sbcl)
(deftype quasiquote-form ()
  `(satisfies quasiquote-form-p))

#+(or (and clasp (not staging)) ecl)
(defun unquote-form-p (form)
  (and (listp form)
       (cdr form)
       (listp (cdr form))
       (null (cddr form))
       (eql (first form)
            #+clasp 'eclector.reader:unquote
            #+ecl 'si:unquote)))

#+(or (and clasp (not staging)) ecl)
(deftype unquote-form ()
  `(satisfies unquote-form-p))

#+(or (and clasp (not staging)) ecl)
(defun unquote-splice-form-p (form)
  (and (listp form)
       (cdr form)
       (listp (cdr form))
       (null (cddr form))
       (eql (first form)
            #+clasp 'eclector.reader:unquote-splicing
            #+ecl 'si:unquote-splice)))

#+(or (and clasp (not staging)) ecl)
(deftype unquote-splice-form ()
  `(satisfies unquote-splice-form-p))

#+ecl
(defun unquote-nsplice-form-p (form)
  (and (listp form)
       (cdr form)
       (null (cddr form))
       (eql (first form)
            'si:unquote-nsplice)))

#+ecl
(deftype unquote-nsplice-form ()
  `(satisfies unquote-nsplice-form-p))

(defun function-quote-form-p (form)
  (and (listp form)
       (cdr form)
       (listp (cdr form))
       (null (cddr form))
       (eql (first form) 'function)))

(deftype function-quote-form ()
  `(satisfies function-quote-form-p))

(defvar +initial-dispatch-entries+
  '(((cons (member apply
                   funcall
                   multiple-value-call))
     0
     pprint-apply)
    ((cons (member case
                   ccase
                   ctypecase
                   ecase
                   etypecase
                   typecase))
     0
     pprint-case)
    ((cons (member cond))
     0
     pprint-cond)
    ((cons (member defclass
                   define-condition))
     0
     pprint-defclass)
    (defmethod-with-qualifier-form
     0
     pprint-defmethod-with-qualifier)
    ((cons (member define-compiler-macro
                   define-modify-macro
                   define-setf-expander
                   defmacro
                   deftype
                   defun))
     0
     pprint-defun)
    ((cons (member do
                   do*))
     0
     pprint-do)
    ((cons (member do-all-symbols
                   do-external-symbols
                   do-symbols
                   dolist
                   dotimes))
     0
     pprint-dolist)
    ((cons (member eval-when
                   multiple-value-setq))
     0
     pprint-eval-when)
    (extended-loop-form
     0
     pprint-extended-loop)
    ((cons (member flet
                   labels
                   macrolet))
     0
     pprint-flet)
    (function-quote-form
     0
     pprint-macro-char :prefix "#'")
    ((cons (member and
                   if
                   or))
     0
     pprint-function-call :newline :linear)
    ((cons (member destructuring-bind))
     0
     pprint-destructuring-bind)
    ((cons (member lambda))
     0
     pprint-lambda)
    ((cons (member let
                   let*))
     0
     pprint-let)
    (simple-loop-form
     0
     pprint-simple-loop)
    (quote-form
     0
     pprint-macro-char :prefix "'")
    ((cons (member block
                   catch
                   defconstant
                   defparameter
                   defvar
                   multiple-value-prog1
                   prog1
                   return-from
                   return
                   throw
                   unless
                   unwind-protect
                   when))
     0
     pprint-prog1)
    ((cons (member prog2))
     0
     pprint-prog2)
    ((cons (member locally
                   progn))
     0
     pprint-progn)
    ((cons (member progv))
     0
     pprint-progv)
    #+(or (and clasp (not staging)) ecl sbcl)
    (quasiquote-form
     0
     pprint-quasiquote :prefix "`" :quote t)
    #+(or (and clasp (not staging)) ecl)
    (unquote-form
     0
     pprint-quasiquote :prefix "," :quote nil)
    #+(or (and clasp (not staging)) ecl)
    (unquote-splice-form
     0
     pprint-quasiquote :prefix ",@" :quote nil)
    #+ecl
    (unquote-nsplice-form
     0
     pprint-quasiquote :prefix ",." :quote nil)
    #+sbcl
    (sb-impl::comma
     0
     pprint-sbcl-comma)
    ((cons (member symbol-macrolet))
     0
     pprint-symbol-macrolet)
    ((cons (member psetf
                   psetq
                   set
                   setf
                   setq))
     0
     pprint-function-call :argument-count 0)
    (pprint-logical-block-form
     0
     pprint-with :argument-count 2)
    (pprint-logical-block-form/2
     0
     pprint-with :argument-count 3)
    ((cons (member with-compilation-unit))
     0
     pprint-with :argument-count 0)
    ((cons (member with-open-stream
                   with-package-iterator
                   with-simple-restart))
     0
     pprint-with)
    ((and array
          (not string)
          (not bit-vector))
     0
     pprint-array)
    (call-form
     -5
     pprint-call)
    (cons
     -10
     pprint-fill t)))

(defvar +extra-dispatch-entries+
  '((symbol                        -10 pprint-symbol)))

(defun add-dispatch-entry (table type-specifier function priority)
  (let ((entry (find type-specifier (dispatch-table-entries table)
                     :test #'equal :key #'dispatch-entry-type-specifier)))
    (if entry
        (setf (dispatch-entry-function entry) function
              (dispatch-entry-priority entry) (or priority 0))
        (push (make-instance 'dispatch-entry
                             :type-specifier type-specifier
                             :function function
                             :priority (or priority 0))
              (dispatch-table-entries table)))
    (setf (dispatch-table-entries table)
          (sort (dispatch-table-entries table) #'> :key #'dispatch-entry-priority)))
  nil)

(defmethod copy-pprint-dispatch (client (table (eql nil)) &optional read-only)
  (declare (ignore table))
  (let ((new-table (make-instance 'dispatch-table)))
    (loop for (type priority name . rest) in +initial-dispatch-entries+
          do (add-dispatch-entry new-table
                                 type
                                 (make-dispatch-function client :client-stream-object
                                                         (fdefinition name) rest)
                                 priority))
    (when read-only
      (setf (dispatch-table-read-only-p new-table) t))
    new-table))

(defmethod copy-pprint-dispatch (client (table (eql t)) &optional read-only)
  (declare (ignore table))
  (let ((new-table (make-instance 'dispatch-table)))
    (loop for (type priority name . rest) in +initial-dispatch-entries+
          do (add-dispatch-entry new-table
                                 type
                                 (make-dispatch-function client :client-stream-object
                                                         (fdefinition name) rest)
                                 priority))
    (loop for (type priority name . rest) in +extra-dispatch-entries+
          do (add-dispatch-entry new-table
                                 type
                                 (make-dispatch-function client :client-stream-object
                                                         (fdefinition name) rest)
                                 priority))
    (when read-only
      (setf (dispatch-table-read-only-p new-table) t))
    new-table))

(defmethod copy-pprint-dispatch (client (table dispatch-table) &optional read-only)
  (declare (ignore client))
  (make-instance 'dispatch-table
                 :read-only (and read-only t)
                 :entries (mapcar (lambda (entry)
                                    (make-instance 'dispatch-entry
                                                   :type-specifier (dispatch-entry-type-specifier entry)
                                                   :function (dispatch-entry-function entry)
                                                   :priority (dispatch-entry-priority entry)))
                                  (dispatch-table-entries table))))

(defmethod pprint-dispatch (client (table dispatch-table) object)
  (when (or (not (arrayp object))
            (and (arrayp object)
                 *print-array*
                 (not *print-readably*)))
    (dolist (entry (dispatch-table-entries table))
      (when (funcall (dispatch-entry-test-function entry) object)
        (return-from pprint-dispatch
                     (values (dispatch-entry-function entry) t)))))
  (values (make-dispatch-function client :client-object-stream #'incless:print-object nil)
          nil))

(defun check-table-read-only (table)
  (when (dispatch-table-read-only-p table)
    (cerror "Ignore and continue"
            "Tried to modify a read-only pprint dispatch table: ~A"
            table)))
  
(defmethod set-pprint-dispatch (client (table dispatch-table) type-specifier (function (eql nil)) priority)
  (declare (ignore client priority))
  (check-table-read-only table)
  (setf (dispatch-table-entries table)
        (delete type-specifier (dispatch-table-entries table)
                :key #'dispatch-entry-type-specifier :test #'equal))
  nil)

(defmethod set-pprint-dispatch (client (table dispatch-table) type-specifier function priority)
  (check-table-read-only table)
  (add-dispatch-entry table type-specifier
                      (make-dispatch-function client :stream-object function nil)
                      priority))
