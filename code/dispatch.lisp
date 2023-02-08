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
   (dispatch-function :accessor dispatch-entry-dispatch-function
                     :initarg :dispatch-function
                     :type function)
   (priority :accessor dispatch-entry-priority
             :initarg :priority
             :initform 0
             :type real)))

(defun cons-names (type-specifier)
  (and (consp type-specifier)
       (eql 'cons (first type-specifier))
       (consp (cdr type-specifier))
       (consp (cadr type-specifier))
       (member (caadr type-specifier) '(eql member))
       (cdadr type-specifier)))

(defun make-test-function (type-specifier)
  (if (or (not (listp type-specifier))
          (not (equal 'cons (car type-specifier))))
      (lambda (object)
        (typep object type-specifier))
      (let* ((car-type (second type-specifier))
             (cdr-type (third type-specifier))
             (cons-type-p (and (consp car-type)
                               (member (car car-type) '(eql member)))))
        (cond ((and cons-type-p cdr-type)
               (lambda (object)
                 (typep object cdr-type)))
              (cons-type-p
               (lambda (object)
                 (declare (ignore object))
                 t))
              (cdr-type
               (lambda (object)
                 (and (consp object)
                      (typep (car object) car-type)
                      (typep (cdr object) cdr-type))))
              (t
               (lambda (object)
                 (and (consp object)
                      (typep (car object) cdr-type))))))))

(defclass dispatch-table ()
  ((entries :accessor dispatch-table-entries
            :initform nil
            :type list)
   (cons-entries :reader dispatch-table-cons-entries
                 :initform (make-hash-table :test 'eql)
                 :type hash-table)
   (non-cons-entries :accessor dispatch-table-non-cons-entries
                     :initform nil
                     :type list)
   (read-only :accessor dispatch-table-read-only-p
              :initarg :read-only
              :initform nil
              :type boolean)))

(defmethod print-object ((entry dispatch-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format stream ":type-specifier ~S :priority ~S"
            (dispatch-entry-type-specifier entry)
            (dispatch-entry-priority entry))))

(defmethod print-object ((table dispatch-table) stream)
  (print-unreadable-object (table stream :type t :identity t)))

(defun call-form-p (form)
  (and form
       (listp form)
       (symbolp (first form))
       (fboundp (first form))))

(deftype call-form ()
  `(satisfies call-form-p))

(defvar +initial-dispatch-entries+
  '(((cons (member apply
                   funcall
                   multiple-value-call))
     -20
     pprint-apply)
    ((cons (member case
                   ccase
                   ctypecase
                   ecase
                   etypecase
                   typecase))
     -20
     pprint-case)
    ((cons (member cond))
     -20
     pprint-cond)
    ((cons (member defclass
                   define-condition))
     -20
     pprint-defclass)
    ((cons (member defmethod)
           (cons t (cons symbol)))
     -10
     pprint-defmethod-with-qualifier)
    ((cons (member defpackage))
     -20
     pprint-defpackage)
    ((cons (member define-compiler-macro
                   define-modify-macro
                   define-setf-expander
                   defmacro
                   defmethod
                   deftype
                   defun))
     -20
     pprint-defun)
    ((cons (member do
                   do*))
     -20
     pprint-do)
    ((cons (member do-all-symbols
                   do-external-symbols
                   do-symbols
                   dolist
                   dotimes))
     -20
     pprint-dolist)
    ((cons (member eval-when
                   multiple-value-setq))
     -20
     pprint-eval-when)
    ((cons (member loop)
           (cons cons))
     -10
     pprint-simple-loop)
    ((cons (member loop))
     -20
     pprint-extended-loop)
    ((cons (member flet
                   labels
                   macrolet))
     -20
     pprint-flet)
    ((cons (member function)
           (cons t null))
     -20
     pprint-macro-char :prefix "#'")
    ((cons (member and
                   if
                   or))
     -20
     pprint-function-call :newline :linear)
    ((cons (member destructuring-bind))
     -20
     pprint-destructuring-bind)
    ((cons (member lambda))
     -20
     pprint-lambda)
    ((cons (member let
                   let*))
     -20
     pprint-let)
    ((cons (member quote)
           (cons t null))
     -20
     pprint-macro-char :prefix "'")
    ((cons (member prog))
     -20
     pprint-prog)
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
     -20
     pprint-prog1)
    ((cons (member prog2))
     -20
     pprint-prog2)
    ((cons (member locally
                   progn))
     -20
     pprint-progn)
    ((cons (member progv))
     -20
     pprint-progv)
    #+(or (and clasp (not staging)) clisp ecl mezzano sbcl)
    ((cons (member #+clasp eclector.reader:quasiquote
                   #+clisp system::backquote
                   #+ecl si:quasiquote
                   #+mezzano mezzano.internals::backquote
                   #+sbcl sb-int:quasiquote)
           (cons t null))
     -20
     pprint-quasiquote :prefix "`" :quote t)
    #+(or (and clasp (not staging)) clisp ecl mezzano)
    ((cons (member #+clasp eclector.reader:unquote
                   #+clisp system::unquote
                   #+ecl si:unquote
                   #+mezzano mezzano.internals::bq-comma)
           (cons t null))
     -20
     pprint-quasiquote :prefix "," :quote nil)
    #+(or (and clasp (not staging)) clisp ecl mezzano)
    ((cons (member #+clasp eclector.reader:unquote-splicing
                   #+clisp system::splice
                   #+ecl si:unquote-splice
                   #+mezzano mezzano.internals::bq-comma-atsign)
           (cons t null))
     -20
     pprint-quasiquote :prefix ",@" :quote nil)
    #+(or clisp ecl mezzano)
    ((cons (member #+clisp system::nsplice
                   #+ecl si:unquote-nsplice
                   #+mezzano mezzano.internals::bq-comma-dot)
           (cons t null))
     -20
     pprint-quasiquote :prefix ",." :quote nil)
    #+sbcl
    (sb-impl::comma
     -20
     pprint-quasiquote)
    ((cons (member symbol-macrolet))
     -20
     pprint-symbol-macrolet)
    ((cons (member psetf
                   psetq
                   set
                   setf
                   setq))
     -20
     pprint-function-call :argument-count 0)
    ((cons (member tagbody))
     -20
     pprint-tagbody)
    ((cons (member cl:pprint-logical-block
                   print-unreadable-object
                   with-input-from-string
                   with-open-file
                   with-output-to-string))
     -20
     pprint-with :argument-count 2)
    ((cons (member pprint-logical-block))
     -20
     pprint-with :argument-count 3)
    ((cons (member with-compilation-unit))
     -20
     pprint-with :argument-count 0)
    ((cons (member with-open-stream
                   with-package-iterator
                   with-simple-restart))
     -20
     pprint-with)
    ((and array
          (not string)
          (not bit-vector))
     -20
     pprint-array)
    (call-form
     -30
     pprint-call)
    (cons
     -40
     pprint-fill t)))

(defvar +extra-dispatch-entries+
  '((symbol
     -10
     pprint-symbol)))

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
    #+(or)(loop for (type priority name . rest) in +extra-dispatch-entries+
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
    (let (entry)
      (when (and (consp object) (symbolp (car object)))
        (loop with cdr = (cdr object)
              for candidate in (gethash (car object) (dispatch-table-cons-entries table))
              when (funcall (dispatch-entry-test-function candidate) cdr)
                do (setf entry candidate)
                   (loop-finish)))
      (loop for candidate in (dispatch-table-non-cons-entries table)
            while (or (not entry)
                      (<= (dispatch-entry-priority entry) (dispatch-entry-priority candidate)))
            when (funcall (dispatch-entry-test-function candidate) object)
              do (setf entry candidate)
                 (loop-finish))
      (when entry
        (return-from pprint-dispatch
                     (values (dispatch-entry-dispatch-function entry) t)))))
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
  (setf (dispatch-table-entries table) (delete type-specifier (dispatch-table-entries table)
                                               :key #'dispatch-entry-type-specifier :test #'equal)
        (dispatch-table-non-cons-entries table) (delete type-specifier (dispatch-table-non-cons-entries table)
                                                        :key #'dispatch-entry-type-specifier :test #'equal))
  (loop with cons-entries = (dispatch-table-cons-entries table)
        for name in (cons-names type-specifier)
        for entries = (delete type-specifier (gethash name cons-entries)
                              :key #'dispatch-entry-type-specifier :test #'equal)
        if entries
          do (setf (gethash name cons-entries) entries)
        else
          do (remhash name cons-entries))
  nil)

(defmethod set-pprint-dispatch (client (table dispatch-table) type-specifier function &optional priority pattern arguments)
  (check-table-read-only table)
  (set-pprint-dispatch client table type-specifier nil)
  (let ((entry (make-instance 'dispatch-entry
                              :type-specifier type-specifier
                              :function function
                              :test-function (make-test-function type-specifier)
                              :dispatch-function (make-dispatch-function client (or pattern :stream-object) function arguments)
                              :priority (or priority 0)
                              :pattern (or pattern :stream-object)
                              :arguments arguments))
        (names (cons-names type-specifier)))
    (setf (dispatch-table-entries table) (sort (cons entry (dispatch-table-entries table))
                                               #'> :key #'dispatch-entry-priority))
    (if names
        (loop with cons-entries = (dispatch-table-cons-entries table)
              for name in names
              do (setf (gethash name cons-entries) (sort (cons entry (gethash name cons-entries))
                                                         #'> :key #'dispatch-entry-priority)))
        (setf (dispatch-table-non-cons-entries table) (sort (cons entry (dispatch-table-non-cons-entries table))
                                                            #'> :key #'dispatch-entry-priority))))
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


