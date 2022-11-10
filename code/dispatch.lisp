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

(defclass dispatch-table ()
  ((entries :accessor dispatch-table-entries
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

(defun apply-form-p (form)
  (and (listp form)
       (member (first form)
               '(apply))))

(deftype apply-form ()
  `(satisfies apply-form-p))

(defun block-form-p (form)
  (and (listp form)
       (member (first form)
               '(block catch defconstant defparameter defvar
                 multiple-value-call multiple-value-prog1
                 print-unreadable-object prog1 return-from throw
                 unless unwind-protect when))))

(deftype block-form ()
  `(satisfies block-form-p))

(defun defclass-form-p (form)
  (and (listp form)
       (member (first form)
               '(defclass define-condition))))

(deftype defclass-form ()
  `(satisfies defclass-form-p))

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

(defun flet-form-p (form)
  (and (listp form)
       (member (first form)
               '(flet labels macrolet))))

(deftype flet-form ()
  `(satisfies flet-form-p))

(defun spread-form-p (form)
  (and (listp form)
       (member (first form)
               '(if and or))))

(deftype spread-form ()
  `(satisfies spread-form-p))

(defun cond-form-p (form)
  (and (listp form)
       (eql (first form) 'cond)))

(deftype cond-form ()
  `(satisfies cond-form-p))

(defun case-form-p (form)
  (and (listp form)
       (member (first form)
               '(case ccase ecase typecase ctypecase etypecase))))

(deftype case-form ()
  `(satisfies case-form-p))

(defun lambda-form-p (form)
  (and (listp form)
       (eql (first form) 'lambda)))

(deftype lambda-form ()
  `(satisfies lambda-form-p))

(defun call-form-p (form)
  (and form
       (listp form)
       (symbolp (first form))))

(deftype call-form ()
  `(satisfies call-form-p))

(defun quote-form-p (form)
  (and (listp form)
       (eql (first form) 'quote)))

(deftype quote-form ()
  `(satisfies quote-form-p))

#+(or clasp ecl sbcl)
(defun quasiquote-form-p (form)
  (and (listp form)
       (eql (first form)
            #+clasp (find-symbol "QUASIQUOTE" :eclector.reader)
            #+ecl 'si:quasiquote
            #+sbcl 'sb-int:quasiquote)))

#+(or clasp ecl sbcl)
(deftype quasiquote-form ()
  `(satisfies quasiquote-form-p))

#+(or clasp ecl)
(defun unquote-form-p (form)
  (and (listp form)
       (eql (first form)
            #+clasp (find-symbol "UNQUOTE" :eclector.reader)
            #+ecl 'si:unquote)))

#+(or clasp ecl)
(deftype unquote-form ()
  `(satisfies unquote-form-p))

#+(or clasp ecl)
(defun unquote-splice-form-p (form)
  (and (listp form)
       (eql (first form)
            #+clasp (find-symbol "UNQUOTE-SPLICING" :eclector.reader)
            #+ecl 'si:unquote-splice)))

#+(or clasp ecl)
(deftype unquote-splice-form ()
  `(satisfies unquote-splice-form-p))

#+ecl
(defun unquote-nsplice-form-p (form)
  (and (listp form)
       (eql (first form)
            'si:unquote-nsplice)))

#+ecl
(deftype unquote-nsplice-form ()
  `(satisfies unquote-nsplice-form-p))

(defun function-quote-form-p (form)
  (and (listp form)
       (eql (first form) 'function)))

(deftype function-quote-form ()
  `(satisfies function-quote-form-p))

(defun setf-form-p (form)
  (and (listp form)
       (member (first form)
               '(psetq set setf setq))))

(deftype setf-form ()
  `(satisfies setf-form-p))

(defvar +default-dispatch-entries+
  '((apply-form                    -10 pprint-apply)
    (block-form                    -10 pprint-block)
    (case-form                     -10 pprint-case)
    (cond-form                     -10 pprint-cond)
    (defclass-form                 -10 pprint-defclass)
    (defmethod-with-qualifier-form -10 pprint-defmethod-with-qualifier)
    (defun-form                    -10 pprint-defun)
    (do-form                       -10 pprint-do)
    (dolist-form                   -10 pprint-dolist)
    (eval-when-form                -10 pprint-eval-when)
    (extended-loop-form            -10 pprint-extended-loop)
    (flet-form                     -10 pprint-flet)
    (function-quote-form           -10 pprint-macro-char :prefix "#'")
    (spread-form                   -10 pprint-function-call :newline :linear)
    (lambda-form                   -10 pprint-lambda)
    (let-form                      -10 pprint-let)
    (pprint-logical-block-form     -10 pprint-with :argument-count 2)
    (pprint-logical-block-form/2   -10 pprint-with :argument-count 3)
    #+(or clasp ecl sbcl)
    (quasiquote-form               -10 pprint-macro-char :prefix "`")
    #+(or clasp ecl)
    (unquote-form                  -10 pprint-macro-char :prefix ",")
    #+(or clasp ecl)
    (unquote-splice-form           -10 pprint-macro-char :prefix ",@")
    #+ecl
    (unquote-nsplice-form          -10 pprint-macro-char :prefix ",.")
    #+sbcl
    (sb-impl::comma                -10 pprint-sbcl-comma)
    (quote-form                    -10 pprint-macro-char :prefix "'")
    (setf-form                     -10 pprint-function-call :argument-count 0)
    (simple-loop-form              -10 pprint-simple-loop)
    (with-compilation-unit-form    -10 pprint-with :argument-count 0)
    (with-hash-table-iterator-form -10 pprint-with)
    ((and array
          (not string)
          (not bit-vector))        -10 pprint-array)
    (call-form                     -20 pprint-call)))

(defun make-dispatch-function (client name rest &aux (func (fdefinition name)))
  (lambda (stream object)
    (handle-circle client (make-pretty-stream client stream) object
                   (lambda (stream object)
                     (apply func client stream object rest)))))

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

(defmethod copy-pprint-dispatch (client (table (eql nil)))
  (declare (ignore client table))
  (let ((new-table (make-instance 'dispatch-table)))
    (loop for (type priority name . rest) in +default-dispatch-entries+
          do (add-dispatch-entry new-table
                                  type
                                  (make-dispatch-function client name rest)
                                  priority))
    new-table))

(defun default-dispatch-print (stream object)
  (print-object object stream))

(defmethod pprint-dispatch (client (table dispatch-table) object)
  (if (and (not *print-array*)
           (arrayp object))
      (values #'default-dispatch-print nil)
      (dolist (entry (dispatch-table-entries table) (values #'default-dispatch-print nil))
        (when (funcall (dispatch-entry-test-function entry) object)
          (return (values (dispatch-entry-function entry) t (dispatch-entry-type-specifier entry)))))))

(defmethod set-pprint-dispatch (client (table dispatch-table) type-specifier (function (eql nil)) priority)
  (setf (dispatch-table-entries table)
        (delete type-specifier (dispatch-table-entries table) :test #'equal))
  nil)

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

(defmethod initialize-instance :after ((instance dispatch-entry) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (dispatch-entry-test-function instance)
        (make-test-function (dispatch-entry-type-specifier instance))))

(defmethod set-pprint-dispatch (client (table dispatch-table) type-specifier function priority)
  (add-dispatch-entry table type-specifier
                      (lambda (stream object)
                        (handle-circle client (make-pretty-stream client stream) object function))
                      priority))

(defvar *print-pprint-dispatch* (copy-pprint-dispatch *client* nil))
