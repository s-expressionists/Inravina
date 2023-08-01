(in-package #:inravina)

(declaim (inline coerce-output-stream-designator))

(defun coerce-output-stream-designator (designator)
  (cond ((null designator)
         *standard-output*)
        ((eq designator t) 
         *terminal-io*)
        ((output-stream-p designator)
         designator)
        (t
         (error 'simple-type-error
                :datum designator
                :expected-type '(satisfies output-stream-p)
                :format-control "~S isn't an output stream."
                :format-arguments (list designator)))))

(defvar *quasiquote* nil)

(defvar *options*
  `(:loop-current-indent-clauses (:as :for :with :initially :finally :do :doing)
    :loop-block-indent-clauses (:if :when :else :unless)
    :loop-top-level-clauses (:always :as :finally :for :initially :named :never
                             :repeat :thereis :until :while :with)
    :loop-compound-clauses (:do :doing :finally :initially)
    :loop-conditional-clauses (:if :when :unless :else)
    :loop-selectable-clauses (:append :appending :collect :collecting :count
                              :counting :do :doing :else :end :if :maximize :maximizing
                              :minimize :minimizing :nconc :nconcing :sum :summing
                              :unless :when)))

(defgeneric make-dispatch-function (client pattern function rest))

(defgeneric copy-pprint-dispatch (client table &optional read-only))

(defgeneric pprint-dispatch (client table object))

(defgeneric set-pprint-dispatch (client table type-specifier function &optional priority pattern arguments))

(defgeneric make-pprint-dispatch-iterator (client table))

(defgeneric pprint-fill (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-linear (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-tabular (client stream object &optional colon-p at-sign-p tabsize))

(defgeneric pprint-indent (client stream relative-to n)
  (:method (client stream relative-to n)
    (declare (ignore stream client relative-to n)))
  (:method :around (client stream relative-to n)
    (declare (ignore client stream relative-to n))
    (when *print-pretty*
      (call-next-method))))
  
(defgeneric pprint-newline (client stream kind)
  (:method (client stream kind)
    (declare (ignore client stream kind)))
  (:method :around (client stream kind)
    (declare (ignore client stream))
    (when (or *print-pretty*
              (member kind '(:mandatory-literal :fresh-literal)))
      (call-next-method))))

(defgeneric pprint-tab (client stream kind colnum colinc)
  (:method (client stream kind colnum colinc)
    (declare (ignore client stream kind colnum colinc)))
  (:method :around (client stream kind colnum colinc)
    (declare (ignore client stream kind colnum colinc))
    (when *print-pretty*
      (call-next-method))))

(defgeneric pprint-fill-plist (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-linear-plist (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-tabular-plist (client stream object &optional colon-p at-sign-p tabsize))
    
(defgeneric pprint-start-logical-block (client stream prefix per-line-prefix-p)
  (:method (client stream prefix per-line-prefix-p)
    (declare (ignore client stream prefix per-line-prefix-p))))

(defgeneric pprint-end-logical-block (client stream suffix)
  (:method (client stream suffix)
    (declare (ignore client stream suffix))))

(defgeneric pprint-valid-list-p (client stream object)
  (:method (client stream object)
    (declare (ignore client))
    (and (listp object)
         (not (and (getf *quasiquote* stream)
                   (unquote-form-p object))))))

(defgeneric make-pretty-stream (client stream)
  (:method (client (stream (eql nil)))
    (call-next-method client *standard-output*))
  (:method (client (stream (eql t)))
    (call-next-method client *terminal-io*)))

(defgeneric pretty-stream-p (client stream)
  (:method (client stream)
    (declare (ignore client stream))
    nil))

(defgeneric break-position (client stream text))

(defgeneric normalize-text (client stream text))

(defgeneric pprint-prog1 (client stream object &rest options &key &allow-other-keys))

;(defgeneric pprint-defun (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-defmethod-with-qualifier (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-do (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-dolist (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-let (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-bindings (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-eval-when (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-progn (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-progv (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-tagbody (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-function-call (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-argument-list (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-lambda-list (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-lambda (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-extended-loop (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-simple-loop (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-array (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-macro-char (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-quasiquote (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-cond (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-case (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-flet (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-with (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-call (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-apply (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-defclass (client stream object &rest options &key &allow-other-keys))

(defgeneric get-named-style (client stream name)
  (:method (client stream name)
    (declare (ignore client stream name))
    nil))

(defmacro with-named-style ((client stream name) &body body)
  (let ((previous-var (gensym))
        (new-var (gensym))
        (body-fun (gensym)))
    `(let ((,previous-var (trivial-stream-column:stream-style ,stream))
           (,new-var (get-named-style ,client ,stream ,name))
           (,body-fun (lambda () ,@body)))
       (cond (,new-var
              (setf (trivial-stream-column:stream-style ,stream) ,new-var)
              (unwind-protect (funcall ,body-fun)
                (setf (trivial-stream-column:stream-style ,stream) ,previous-var)))
             (t
              (funcall ,body-fun))))))

(defmacro define-interface ((client-var client-class &optional intrinsic) &body body)
  (let* ((intrinsic-pkg (if intrinsic (find-package "COMMON-LISP") *package*))
         (initial-pprint-dispatch-var (intern "*INITIAL-PPRINT-DISPATCH*"))
         (standard-pprint-dispatch-var (intern "*STANDARD-PPRINT-DISPATCH*"))
         (print-pprint-dispatch-var (intern "*PRINT-PPRINT-DISPATCH*" intrinsic-pkg))
         (pprint-pop-func (intern "PPRINT-POP" intrinsic-pkg))
         (pprint-exit-if-list-exhausted-func (intern "PPRINT-EXIT-IF-LIST-EXHAUSTED" intrinsic-pkg))
         (initialize-func (intern "INITIALIZE")))
    `(progn
       (defmethod make-dispatch-function
           ((client ,client-class) (pattern (eql :client-stream-object)) function rest)
         (lambda (stream object)
           (apply function ,client-var (make-pretty-stream ,client-var stream) object rest)))
       (defmethod make-dispatch-function
           ((client ,client-class) (pattern (eql :client-object-stream)) function rest)
         (lambda (stream object)
           (apply function ,client-var object (make-pretty-stream ,client-var stream) rest)))
       (defmethod make-dispatch-function
           ((client ,client-class) (pattern (eql :stream-object)) function rest)
         (lambda (stream object)
           (apply function (make-pretty-stream ,client-var stream) object rest)))
       (defmethod make-dispatch-function
           ((client ,client-class) (pattern (eql :object-stream)) function rest)
         (lambda (stream object)
           (apply function object (make-pretty-stream ,client-var stream) rest)))
       (defvar ,initial-pprint-dispatch-var nil)
       (defvar ,standard-pprint-dispatch-var nil)
       (defvar ,print-pprint-dispatch-var)
       (defun ,(intern "PRETTY-STREAM-P") (stream)
         (pretty-stream-p ,client-var stream))
       (defun ,(intern "COPY-PPRINT-DISPATCH" intrinsic-pkg) (&optional (table ,print-pprint-dispatch-var))
         #+ecl ,@(when intrinsic '((declare (ext:check-arguments-type nil))))
         (check-type table (or null dispatch-table))
         (copy-pprint-dispatch ,client-var (or table ,initial-pprint-dispatch-var)))
       (defun ,(intern "SET-PPRINT-DISPATCH" intrinsic-pkg)
           (type-specifier function &optional (priority 0) (table ,print-pprint-dispatch-var))
         #+ecl ,@(when intrinsic '((declare (ext:check-arguments-type nil))))
         (check-type priority real)
         (check-type table dispatch-table)
         (check-type function (or symbol function))
         (set-pprint-dispatch ,client-var table type-specifier function priority))
       (defun ,(intern "PPRINT-FILL" intrinsic-pkg) (stream object &optional (colon-p t) at-sign-p)
         (pprint-fill ,client-var (coerce-output-stream-designator stream)
                               object colon-p at-sign-p)
         nil)
       (defun ,(intern "PPRINT-LINEAR" intrinsic-pkg) (stream object &optional (colon-p t) at-sign-p)
         (pprint-linear ,client-var (coerce-output-stream-designator stream)
                                 object colon-p at-sign-p)
         nil)
       (defun ,(intern "PPRINT-TABULAR" intrinsic-pkg) (stream object &optional (colon-p t) at-sign-p (tabsize 16))
         (pprint-tabular ,client-var (coerce-output-stream-designator stream)
                                  object colon-p at-sign-p tabsize)
         nil)
       (defun ,(intern "PPRINT-INDENT" intrinsic-pkg) (relative-to n &optional stream)
         (check-type relative-to (member :block :current))
         (pprint-indent ,client-var (coerce-output-stream-designator stream)
                                 relative-to n)
         nil)
       (defun ,(intern "PPRINT-NEWLINE" intrinsic-pkg) (kind &optional stream)
         (check-type kind (member :linear :fill :miser :mandatory))
         (pprint-newline ,client-var (coerce-output-stream-designator stream)
                                  kind)
         nil)
       (defun ,(intern "PPRINT-TAB" intrinsic-pkg) (kind colnum colinc &optional stream)
         (check-type kind (member :line :section :line-relative :section-relative))
         (pprint-tab ,client-var (coerce-output-stream-designator stream)
                              kind colnum colinc)
         nil)
       (defun ,(intern "PPRINT-DISPATCH" intrinsic-pkg) (object &optional (table ,print-pprint-dispatch-var))
         #+ecl ,@(when intrinsic '((declare (ext:check-arguments-type nil))))
         (check-type table (or null dispatch-table))
         (pprint-dispatch ,client-var (or table ,initial-pprint-dispatch-var) object))
       (defmacro ,(intern "PPRINT-LOGICAL-BLOCK" intrinsic-pkg) ((stream-symbol object
                                        &key (prefix "" prefix-p)
                                          (per-line-prefix "" per-line-prefix-p)
                                          (suffix "" suffix-p))
                                       &body body)
         (expand-logical-block ',client-var stream-symbol object
                                        prefix prefix-p per-line-prefix per-line-prefix-p suffix suffix-p
                                        ',pprint-exit-if-list-exhausted-func ',pprint-pop-func
                                        body))
       (defmacro ,pprint-exit-if-list-exhausted-func ()
         "Tests whether or not the list passed to the lexically current logical block has
 been exhausted. If this list has been reduced to nil, pprint-exit-if-list-exhausted
 terminates the execution of the lexically current logical block except for the
 printing of the suffix. Otherwise pprint-exit-if-list-exhausted returns nil."
         (error "PPRINT-EXIT-IF-LIST-EXHAUSTED must be lexically inside PPRINT-LOGICAL-BLOCK."))
       (defmacro ,pprint-pop-func ()
         "Pops one element from the list being printed in the lexically current logical
 block, obeying *print-length* and *print-circle*."
         (error "PPRINT-POP must be lexically inside PPRINT-LOGICAL-BLOCK."))
       (defun ,initialize-func (&aux *print-pretty*)
         (find-unquote-symbols)
         (setf ,initial-pprint-dispatch-var (copy-pprint-dispatch ,client-var nil t)
               ,standard-pprint-dispatch-var (copy-pprint-dispatch ,client-var nil t)
               ,print-pprint-dispatch-var (copy-pprint-dispatch ,client-var nil))
         ,@body))))

(defgeneric execute-logical-block (client stream object function
                                   &key prefix per-line-prefix-p suffix))
