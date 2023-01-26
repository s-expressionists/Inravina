(in-package #:inravina)

(defclass dispatch-entry ()
  ((type-specifier :accessor dispatch-entry-type-specifier
                   :initarg :type-specifier)
   (test-function :accessor dispatch-entry-test-function
                  :initarg :test-function
                  :type function)
   (function :accessor dispatch-entry-function
             :initarg :function
             :type function)
   (pattern :accessor dispatch-entry-pattern
            :initarg :pattern
            :type (member :client-stream-object :client-object-stream :stream-object :object-stream))
   (arguments :accessor dispatch-entry-arguments
              :initarg :arguments
              :type list)
   (wrapped-function :accessor dispatch-entry-wrapped-function
                     :initarg :wrapped-function
                     :type function)
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

(defmethod copy-pprint-dispatch (client (table (eql nil)) &optional read-only)
  (declare (ignore table))
  (let ((new-table (make-instance 'dispatch-table)))
    (loop for (type priority name . rest) in +initial-dispatch-entries+
          do (set-pprint-dispatch client new-table type (fdefinition name) priority :client-stream-object rest))
    (when read-only
      (setf (dispatch-table-read-only-p new-table) t))
    new-table))

(defmethod copy-pprint-dispatch (client (table (eql t)) &optional read-only)
  (declare (ignore table))
  (let ((new-table (make-instance 'dispatch-table)))
    (loop for (type priority name . rest) in +initial-dispatch-entries+
          do (set-pprint-dispatch client new-table type (fdefinition name) priority :client-stream-object rest))
    (loop for (type priority name . rest) in +extra-dispatch-entries+
          do (set-pprint-dispatch client new-table type (fdefinition name) priority :client-stream-object rest))
    (when read-only
      (setf (dispatch-table-read-only-p new-table) t))
    new-table))

(defmethod copy-pprint-dispatch (client table &optional read-only)
  (loop with iterator = (make-pprint-dispatch-iterator client table)
        with new-table = (make-instance 'dispatch-table)
        for (presentp type-specifier function priority pattern arguments) = (multiple-value-list (funcall iterator))
        finally (setf (dispatch-table-read-only-p new-table) read-only)
                (return new-table)
        while presentp
        do (set-pprint-dispatch client new-table type-specifier function priority pattern arguments)))

(defmethod pprint-dispatch (client (table dispatch-table) object)
  (when (or (not (arrayp object))
            (and (arrayp object)
                 *print-array*
                 (not *print-readably*)))
    (dolist (entry (dispatch-table-entries table))
      (when (funcall (dispatch-entry-test-function entry) object)
        (return-from pprint-dispatch
                     (values (dispatch-entry-wrapped-function entry) t)))))
  (values (make-dispatch-function client :client-object-stream #'incless:print-object nil)
          nil))

(defun check-table-read-only (table)
  (when (dispatch-table-read-only-p table)
    (cerror "Ignore and continue"
            "Tried to modify a read-only pprint dispatch table: ~A"
            table)))
  
(defmethod set-pprint-dispatch (client (table dispatch-table) type-specifier (function (eql nil)) &optional priority pattern arguments)
  (declare (ignore client priority pattern arguments))
  (check-table-read-only table)
  (setf (dispatch-table-entries table)
        (delete type-specifier (dispatch-table-entries table)
                :key #'dispatch-entry-type-specifier :test #'equal))
  nil)

(defmethod set-pprint-dispatch (client (table dispatch-table) type-specifier function &optional priority pattern arguments)
  (check-table-read-only table)
  (set-pprint-dispatch client table type-specifier nil)
  (setf (dispatch-table-entries table)
        (sort (cons (make-instance 'dispatch-entry
                                   :type-specifier type-specifier
                                   :function function
                                   :test-function (make-test-function type-specifier)
                                   :wrapped-function (make-dispatch-function client (or pattern :stream-object) function arguments)
                                   :priority (or priority 0)
                                   :pattern (or pattern :stream-object)
                                   :arguments arguments)
                    (dispatch-table-entries table))
              #'> :key #'dispatch-entry-priority))
  nil)

(defmethod make-pprint-dispatch-iterator (client (table dispatch-table))
  (declare (ignore client))
  (let ((entries (dispatch-table-entries table)))
    (lambda ()
      (if entries
          (let ((entry (pop entries)))
            (values t
                    (dispatch-entry-type-specifier entry)
                    (dispatch-entry-function entry)
                    (dispatch-entry-priority entry)
                    (dispatch-entry-pattern entry)
                    (dispatch-entry-arguments entry)))
          (values nil nil nil nil nil nil)))))


