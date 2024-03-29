(in-package #:inravina)

;; miser -> fill -> linear -> mandatory
;; or
;; miser/fill -> linear -> mandatory
;;
;;           |           |    @      |    :      |    :@     |
;; ----------|-----------|-----------|-----------|-----------|
;; miser     | miser     | fill      | linear    | mandatory |
;; fill      | fill      | linear    | linear    | mandatory |
;; linear    | linear    | linear    | linear    | mandatory |
;; mandatory | mandatory | mandatory | mandatory | mandatory |
;;
;; or
;;           |           |    @      |    :      |    :@     |
;; ----------|-----------|-----------|-----------|-----------|
;; miser     | miser     | miser     | linear    | mandatory |
;; fill      | fill      | linear    | linear    | mandatory |
;; linear    | linear    | linear    | linear    | mandatory |
;; mandatory | mandatory | mandatory | mandatory | mandatory |


(defmacro with-newlines (&body body)
  `(let ((miser-newline (cond ((not colon-p)
                               :miser)
                              (at-sign-p
                               :mandatory)
                              (t
                               :linear)))
         (fill-newline (cond ((and colon-p at-sign-p)
                              :mandatory)
                             ((or colon-p at-sign-p)
                              :linear)
                             (t
                              :fill)))
         (linear-newline (if (and colon-p at-sign-p)
                             :mandatory
                             :linear)))
     (declare (ignorable miser-newline fill-newline linear-newline))
     ,@body))

(defun lambda-list (sym)
  #+ccl
    (ccl:arglist sym)
  #+clisp
    (system::arglist sym)
  #+(or clasp ecl)
    (with-standard-io-syntax (ext:function-lambda-list sym))
  #+sbcl
    (sb-introspect:function-lambda-list sym)
  #+lispworks
    (lispworks:function-lambda-list sym)
  #-(or ccl clisp clasp ecl lispworks sbcl)
    (unless (special-operator-p sym)
      (second (function-lambda-expression (or (macro-function sym)
                                              (fdefinition sym))))))

(defun parse-lambda-list (lambda-list)
  (loop with required = t
        for token = (and (listp lambda-list) (car lambda-list))
        while lambda-list
        when (or (symbolp lambda-list) ; dotted &rest type
                 (eq token '&aux))
          do (return (values patterns nil))
        else when (member token '(&body &key)) ; plist or body
          do (return (values patterns token))
        else when (member token '(&environment &rest &whole)) ; ignore
          do (pop lambda-list)
        else when (eq token '&optional) ; prevent collection of destructuring lambdas
          do (setf required nil)
        else ; collect destructuring lambda if appropriate
          collect (and required (listp token) token) into patterns
        do (pop lambda-list)
        finally (return (values patterns nil))))

(defmacro pprint-body-form ((client stream object &key newline) &body body)
  (let ((newline-var (gensym)))
    `(let ((,newline-var (or ,newline :linear)))
       (pprint-list (,client ,stream ,object :paren t :newline ,newline-var)
         ,@body
         (pprint-exit-if-list-exhausted)
         (pprint-indent ,client ,stream :block 1)
         (write-char #\Space ,stream)
         (pprint-newline ,client ,stream ,newline-var)))))

(defmacro pprint-tagbody-form ((client stream object &key newline) &body body)
  (let ((newline-var (gensym)))
    `(pprint-format-logical-block (,client ,stream ,object :paren t)
       ,@body
       (pprint-exit-if-list-exhausted)
       (loop with ,newline-var = (or ,newline :linear)
             for form-or-tag = (pprint-pop)
             do (pprint-indent ,client ,stream :block
                               (if (atom form-or-tag) 0 1))
                (write-char #\Space ,stream)
                (pprint-newline ,client ,stream ,newline-var)
                (incless:write-object ,client form-or-tag ,stream)
                (pprint-exit-if-list-exhausted)))))

(defmacro pprint-function-call-form ((client stream object &key argument-count newline) &body body)
  `(pprint-plist (,client ,stream ,object :paren t :newline (or ,newline :fill))
     ,@body
     (pprint-exit-if-list-exhausted)
     (loop for i below (or ,argument-count most-positive-fixnum)
           do (incless:write-object ,client (pprint-pop) ,stream)
              (pprint-exit-if-list-exhausted)
              (write-char #\Space ,stream)
              (pprint-newline ,client ,stream (or ,newline :fill)))))

(defmethod pprint-bindings (client stream object &optional colon-p at-sign-p)
  (pprint-format-logical-block (client stream object :paren colon-p)
    (pprint-exit-if-list-exhausted)
    (loop with newline = (if at-sign-p :fill :linear)
          do (pprint-fill client stream (pprint-pop) t nil)
             (pprint-exit-if-list-exhausted)
             (write-char #\Space stream)
             (pprint-newline client stream newline))))

(defmethod pprint-defpackage (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-logical-block (client stream object :prefix "(" :suffix ")")
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (incless:write-object client (pprint-pop) stream)
      (pprint-indent client stream :block 1)
      (loop (pprint-exit-if-list-exhausted)
            (write-char #\Space stream)
            (pprint-newline client stream linear-newline)
            (pprint-function-call client stream (pprint-pop) at-sign-p)))))

(defmethod pprint-defun (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline client stream fill-newline)
      (pprint-lambda-list client stream (pprint-pop) colon-p at-sign-p))))

(defmethod pprint-destructuring-bind (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (pprint-lambda-list client stream (pprint-pop) colon-p at-sign-p)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline client stream fill-newline)
      (incless:write-object client (pprint-pop) stream))))

(defmethod pprint-defmethod (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline client stream fill-newline)
      (let ((arg (pprint-pop)))
        (cond ((listp arg)
               (pprint-lambda-list client stream arg colon-p at-sign-p))
              (t
               (incless:write-object client arg stream)
               (pprint-exit-if-list-exhausted)
               (write-char #\Space stream)
               (pprint-newline client stream fill-newline)
               (pprint-lambda-list client stream (pprint-pop) colon-p at-sign-p)))))))

(defmethod pprint-do (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-tagbody-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (pprint-indent client stream :current 0)
      (pprint-bindings client stream (pprint-pop) t)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline client stream fill-newline)
      (pprint-linear client stream (pprint-pop) t nil))))

(defmethod pprint-dolist (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-tagbody-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (pprint-fill client stream (pprint-pop) t nil))))

(defmethod pprint-eval-when (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (pprint-fill client stream (pprint-pop) t nil))))

(defmethod pprint-multiple-value-bind (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (pprint-fill client stream (pprint-pop) t nil)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline client stream fill-newline)
      (incless:write-object client (pprint-pop) stream))))

(defmethod pprint-let (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-indent client stream :block 3)
      (pprint-newline client stream miser-newline)
      (pprint-bindings client stream (pprint-pop) t))))

(defmethod pprint-prog (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-tagbody-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-indent client stream :current 0)
      (pprint-newline client stream miser-newline)
      (pprint-bindings client stream (pprint-pop) t))))

(defmethod pprint-prog1 (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (incless:write-object client (pprint-pop) stream))))

(defmethod pprint-prog2 (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline client stream fill-newline)
      (incless:write-object client (pprint-pop) stream))))

(defmethod pprint-progn (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream))))

(defmethod pprint-progv (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (pprint-linear client stream (pprint-pop) t nil)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline client stream fill-newline)
      (pprint-linear client stream (pprint-pop) t nil))))

(defmethod pprint-tagbody (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-tagbody-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream))))

(defmethod pprint-function-call (client stream object
                                 &optional colon-p at-sign-p argument-count)
  (with-newlines
    (pprint-function-call-form (client stream object :argument-count argument-count
                                                     :newline fill-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-indent client stream :current 0))))

(defmethod pprint-argument-list (client stream object
                                 &optional colon-p at-sign-p argument-count)
  (with-newlines
    (pprint-function-call-form (client stream object :argument-count argument-count
                                                     :newline fill-newline))))

(defmethod pprint-with (client stream object &optional colon-p at-sign-p argument-count)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (pprint-argument-list client stream (pprint-pop) colon-p at-sign-p argument-count))))

(defmethod pprint-lambda-list (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-format-logical-block (client stream object :paren t)
      (let ((state :required)
            (first t)
            (group nil))
        (loop (pprint-exit-if-list-exhausted)
              (unless first
                (write-char #\Space stream))
              (when group
                (pprint-indent client stream :current 0)
                (setf group nil))
              (let ((arg (pprint-pop)))
                (unless first
                  (case arg
                    (&optional
                     (setf state :optional
                           group t)
                     (pprint-indent client stream :block 0)
                     (pprint-newline client stream linear-newline))
                    ((&rest &body)
                     (setf state :required
                           group t)
                     (pprint-indent client stream :block 0)
                     (pprint-newline client stream linear-newline))
                    (&key
                     (setf state :key
                           group t)
                     (pprint-indent client stream :block 0)
                     (pprint-newline client stream linear-newline))
                    (&aux
                     (setf state :optional
                           group t)
                     (pprint-indent client stream :block 0)
                     (pprint-newline client stream linear-newline))
                    (otherwise
                     (pprint-newline client stream fill-newline))))
                (cond
                  ((eq state :required)
                   (pprint-lambda-list client stream arg colon-p at-sign-p))
                  (colon-p
                   (pprint-fill client stream arg t))
                  (t
                   (pprint-linear client stream arg t))))
              (setf first nil))))))

(defun loop-top-level-clause-p (item)
  (and (symbolp item)
       (member item (getf *options* :loop-top-level-clauses) :test #'string=)))

(defun loop-conditional-clause-p (item)
  (and (symbolp item)
       (member item (getf *options* :loop-conditional-clauses) :test #'string=)))

(defun loop-compound-clause-p (item)
  (and (symbolp item)
       (member item (getf *options* :loop-compound-clauses) :test #'string=)))

(defun loop-selectable-clause-p (item)
  (and (symbolp item)
       (member item (getf *options* :loop-selectable-clauses) :test #'string=)))

(defun loop-newline-p (item parent)
  (and (or (loop-top-level-clause-p item)
           (loop-selectable-clause-p item)
           (and (symbolp parent)
                (and (loop-compound-clause-p parent)
                     (listp item)))
           (and (symbolp item)
                (string= item :and)))
       (not (and (symbolp parent)
                 (string= parent :else)
                 (symbolp item)
                 (member item '(:if :when :unless) :test #'string=)))))

(defun loop-start-clause-p (item parent)
  (and (or (loop-top-level-clause-p item)
           (loop-selectable-clause-p item))
       (not (and (symbolp parent)
                 (string= parent :else)
                 (symbolp item)
                 (member item '(:if :when :unless) :test #'string=)))))

(defun loop-end-clause-p (item parent)
  (or (and (loop-selectable-clause-p parent)
           (symbolp item)
           (string= item :and))
      #+(or)(and (symbolp item)
           (string= item :end))))

(defun loop-end-all-clauses-p (item parent)
  (or (loop-top-level-clause-p item)
      (and (not (loop-conditional-clause-p parent))
           (loop-selectable-clause-p item))))

(defun loop-block-indent-p (item)
  (and (symbolp item)
       (member item (getf *options* :loop-block-indent-clauses)
               :test #'string=)))

(defun loop-current-indent-p (item)
  (and (symbolp item)
       (member item (getf *options* :loop-current-indent-clauses)
               :test #'string=)))

(defun allow-break-p (item parent)
  (or (not (symbolp item))
      (not (member item '(:and :do :doing :finally :initially)
                   :test #'string=))
      (and (string= item :and)
           (member parent '(:as :for :with)))))

(defmethod pprint-loop (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (if (consp (second object))
        (pprint-list (client stream object :paren t :newline linear-newline)
          (pprint-exit-if-list-exhausted)
          (incless:write-object client (pprint-pop) stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-indent client stream :current 0))
        (pprint-format-logical-block (client stream object :paren t)
          (pprint-exit-if-list-exhausted)
          (incless:write-object client (pprint-pop) stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-indent client stream :current 0)
          (let (allow-break-p clauses)
            (flet ((do-newline ()
                     (if allow-break-p
                         (pprint-newline client stream linear-newline)
                         (setf allow-break-p t)))
                   (end-all-clauses ()
                     (loop while clauses
                           do (pop clauses)
                              (pprint-end-logical-block client stream ""))))
              (unwind-protect
                   (loop for item = (pprint-pop)
                         for parent = (car clauses)
                         when (loop-end-clause-p item parent)
                           do (pop clauses)
                              (pprint-end-logical-block client stream "")
                         when (loop-end-all-clauses-p item parent)
                           do (end-all-clauses)
                         when (loop-newline-p item parent)
                           do (do-newline)
                         when (loop-start-clause-p item parent)
                           do (pprint-start-logical-block client stream "" nil)
                              (push item clauses)
                         do (incless:write-object client item stream)
                            (pprint-exit-if-list-exhausted)
                            (write-char #\Space stream)
                         when (loop-block-indent-p item)
                           do (pprint-indent client stream :block 2)
                         when (loop-current-indent-p item)
                           do (pprint-indent client stream :current 0)
                         do (setf allow-break-p (allow-break-p item parent)))
                (end-all-clauses))))))))

(defun literal-array-p (arr)
  (and (eq t (array-element-type arr))
       (loop for (dimension . tail) on (array-dimensions arr)
             when (and tail
                       (zerop dimension))
               return nil
             unless tail
               return t)))

(defmethod pprint-array (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (cond ((and *print-readably*
                (not (literal-array-p object)))
           (incless:print-object client stream object))
          ((vectorp object)
           (pprint-logical-block (client stream nil :prefix "#(" :suffix ")")
             (dotimes (pos (length object))
               (unless (zerop pos)
                 (write-char #\Space stream)
                 (pprint-newline client stream fill-newline))
               (pprint-pop)
               (incless:write-object client (aref object pos) stream))))
          ((array-dimensions object)
           (labels ((pprint-subarray (index dimensions)
                      (pprint-logical-block (client stream nil
                                                    :prefix (if (= (length dimensions)
                                                                   (array-rank object))
                                                                (with-output-to-string (s)
                                                                  (write-char #\# s)
                                                                  (let ((*print-radix* nil)
                                                                        (*print-base* 10))
                                                                    (incless:write-object client (array-rank object) s))
                                                                  (write-string "A(" s))
                                                                "(")
                                                    :suffix ")")
                        (loop with dimension = (car dimensions)
                              with remaining-dimensions = (cdr dimensions)
                              for pos below dimension
                              for new-index = (+ pos (* index dimension))
                              unless (zerop pos)
                                do (write-char #\Space stream)
                                   (pprint-newline client stream fill-newline)
                              do (pprint-pop)
                              if remaining-dimensions
                                do (pprint-subarray new-index remaining-dimensions)
                              else
                                do (incless:write-object client (row-major-aref object new-index) stream)))))
             (pprint-subarray 0 (array-dimensions object))))
          (t
           (write-string "#0A" stream)
           (incless:write-object client (aref object) stream)))))

(defmethod pprint-lambda (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (pprint-lambda-list client stream (pprint-pop) colon-p at-sign-p))))

(defmethod pprint-macro-char (client stream object &optional quasiquote-p unquote-p disp-char sub-char)
  (cond ((and quasiquote-p unquote-p (not (getf *quasiquote* stream)))
         (pprint-fill client stream object t))
        (t
         (when disp-char
           (write-char disp-char stream))
         (when sub-char
           (write-char sub-char stream))
         (if quasiquote-p
             (let ((*quasiquote* (list* stream (not unquote-p) *quasiquote*)))
               (incless:write-object client (second object) stream))
             (incless:write-object client (second object) stream)))))

#+sbcl
(defun pprint-sbcl-quasiquote (client stream object)
  (cond ((getf *quasiquote* stream)
         (write-string (ecase (sb-impl::comma-kind object)
                         (0 ",")
                         (1 ",.")
                         (2 ",@"))
                       stream)
         (let ((*quasiquote* (list* stream nil *quasiquote*)))
           (incless:write-object client (sb-impl::comma-expr object) stream)))
        (t
         (incless:print-object client object stream))))

(defmethod pprint-case (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-format-logical-block (client stream object :paren t)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-indent client stream :block 3)
      (pprint-newline client stream miser-newline)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 1)
      (write-char #\Space stream)
      (pprint-newline client stream linear-newline)
      (loop (pprint-linear client stream (pprint-pop) t nil)
            (pprint-exit-if-list-exhausted)
            (write-char #\Space stream)
            (pprint-newline client stream linear-newline)))))

(defmethod pprint-cond (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-format-logical-block (client stream object :paren t)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (pprint-indent client stream :current 0)
      (loop (pprint-linear client stream (pprint-pop) t nil)
            (pprint-exit-if-list-exhausted)
            (write-char #\Space stream)
            (pprint-newline client stream linear-newline)))))

(defmethod pprint-flet (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (pprint-format-logical-block (client stream (pprint-pop) :paren t)
        (pprint-exit-if-list-exhausted)
        (pprint-lambda client stream (pprint-pop))
        (pprint-exit-if-list-exhausted)
        (write-char #\Space stream)
        (pprint-newline client stream linear-newline)))))

(defmethod pprint-call (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (if (fboundp (first object))
        (let ((macrop (and (macro-function (first object)) t)))
          (flet ((write-object-pattern (client stream object pattern)
                   (if (and macrop pattern)
                       (pprint-argument-list client stream object t nil
                                             (multiple-value-bind (sub-patterns sub-terminator)
                                                 (parse-lambda-list pattern)
                                               (when sub-terminator
                                                 (length sub-patterns))))
                       (incless:write-object client object stream))))
            (multiple-value-bind (patterns terminator)
                (parse-lambda-list (lambda-list (first object)))
              (case terminator
                (&body
                 (pprint-body-form (client stream object)
                   (pprint-exit-if-list-exhausted)
                   (incless:write-object client (pprint-pop) stream)
                   (pprint-exit-if-list-exhausted)
                   (write-char #\Space stream)
                   (pprint-indent client stream :block 3)
                   (pprint-newline client stream miser-newline)
                   (loop for pattern in patterns
                         for index from 0
                         when (plusp index)
                           do (write-char #\Space stream)
                              (pprint-newline client stream linear-newline)
                         do (write-object-pattern client stream (pprint-pop) pattern)
                            (pprint-exit-if-list-exhausted))))
                (&key
                 (pprint-plist (client stream object :paren t :newline fill-newline)
                   (pprint-exit-if-list-exhausted)
                   (incless:write-object client (pprint-pop) stream)
                   (pprint-exit-if-list-exhausted)
                   (write-char #\Space stream)
                   (pprint-indent client stream :current 0)
                   (loop for pattern in patterns
                         do (write-object-pattern client stream (pprint-pop) pattern)
                            (pprint-exit-if-list-exhausted)
                            (write-char #\Space stream)
                            (pprint-newline client stream fill-newline))))
                (otherwise
                 (pprint-list (client stream object :paren t :newline fill-newline)
                   (pprint-exit-if-list-exhausted)
                   (incless:write-object client (pprint-pop) stream)
                   (pprint-exit-if-list-exhausted)
                   (write-char #\Space stream)
                   (pprint-indent client stream :current 0)
                   (loop for pattern in patterns
                         do (write-object-pattern client stream (pprint-pop) pattern)
                            (pprint-exit-if-list-exhausted)
                            (write-char #\Space stream)
                            (pprint-newline client stream fill-newline))))))))
        (pprint-function-call client stream object t))))

(defmethod pprint-apply (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-function-call-form (client stream object)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-indent client stream :current 0)
      (pprint-newline client stream fill-newline))))

(defmethod pprint-defclass (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-format-logical-block (client stream object :paren t)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (incless:write-object client (pprint-pop) stream)
      ;; superclasses
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline client stream fill-newline)
      (pprint-fill client stream (pprint-pop) t)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 1)
      (write-char #\Space stream)
      (pprint-newline client stream linear-newline)
      ;; slots
      (pprint-format-logical-block (client stream (pprint-pop) :paren t)
        (pprint-exit-if-list-exhausted)
        (loop do (pprint-function-call client stream (pprint-pop) colon-p at-sign-p 0)
                 (pprint-exit-if-list-exhausted)
                 (write-char #\Space stream)
                 (pprint-newline client stream linear-newline)))
      ;; options
      (pprint-exit-if-list-exhausted)
      (pprint-newline client stream linear-newline)
      (loop do (pprint-function-call client stream (pprint-pop) colon-p at-sign-p 0)
               (pprint-exit-if-list-exhausted)
               (write-char #\Space stream)
               (pprint-newline client stream linear-newline)))))

(defun muffp (symbol char &aux (name (symbol-name symbol)))
  (and (> (length name) 1)
       (char= (char name 0) char)
       (char= (char name (1- (length name))) char)))

(defun constant-variable-p (symbol)
  (or (and (boundp symbol)
           (constantp symbol))
      (muffp symbol #\+)))

(defun dynamic-variable-p (symbol)
  (or (and (boundp symbol)
           (not (constantp symbol)))
      (muffp symbol #\*)))

(defun pprint-symbol (client stream object &rest options &key &allow-other-keys)
  (declare (ignore options))
  (with-style (client stream
               :name (cond ((constant-variable-p object)
                            :constant-variable)
                           ((dynamic-variable-p object)
                            :dynamic-variable)
                           (t
                            nil)))
    (incless:print-object client object stream)))

(defmethod pprint-symbol-macrolet (client stream object &optional colon-p at-sign-p)
  (with-newlines
    (pprint-body-form (client stream object :newline linear-newline)
      (pprint-exit-if-list-exhausted)
      (incless:write-object client (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (pprint-indent client stream :block 3)
      (write-char #\Space stream)
      (pprint-newline client stream miser-newline)
      (pprint-format-logical-block (client stream (pprint-pop) :paren t)
        (pprint-exit-if-list-exhausted)
        (pprint-progn client stream (pprint-pop) colon-p at-sign-p)
        (pprint-exit-if-list-exhausted)
        (write-char #\Space stream)
        (pprint-newline client stream linear-newline)))))
