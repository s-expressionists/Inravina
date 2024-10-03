(in-package #:inravina)

(declaim (inline coerce-output-stream-designator
                 ensure-symbol))

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

(defun ensure-symbol (name &optional (package *package*))
  (intern (string name) package))

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
    (when *print-pretty*
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
    
(defgeneric pprint-start-logical-block (client stream object prefix per-line-prefix-p)
  (:method (client stream object prefix per-line-prefix-p)
    (declare (ignore client stream object prefix per-line-prefix-p))))

(defgeneric pprint-end-logical-block (client stream object suffix)
  (:method (client stream object suffix)
    (declare (ignore client stream object suffix))))

(defgeneric layout-block (client stream object kind line column)
  (:method (client stream object kind line column)
    (declare (ignore client stream object kind line column))))
           
(defgeneric pprint-valid-list-p (client stream object)
  (:method (client stream object)
    (declare (ignore client))
    (and (listp object)
         (not (quasiquote-form-p object))
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

(defgeneric pprint-array (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-apply (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-bindings (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-call (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-case (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-cond (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-defclass (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-defun (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-defmethod (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-defpackage (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-do (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-dolist (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-flet (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-lambda (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-let (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-loop (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-multiple-value-bind (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-prog (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-prog1 (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-prog2 (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-progn (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-progv (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-symbol-macrolet (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-tagbody (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-function-call (client stream object &optional colon-p at-sign-p argument-count))

(defgeneric pprint-argument-list (client stream object &optional colon-p at-sign-p argument-count))

(defgeneric pprint-with (client stream object &optional colon-p at-sign-p argument-count))

(defgeneric pprint-lambda-list (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-destructuring-bind (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-macro-char (client stream object &optional quasiquote-p unquote-p disp-char sub-char))

(defgeneric stream-style (stream)
  (:method (stream)
    (declare (ignore stream))
    nil))

(defgeneric (setf stream-style) (new-style stream)
  (:method (new-style stream)
    (declare (ignore stream))
    new-style))

(defgeneric make-style (client stream &rest initargs &key)
  (:method (client stream &rest initargs &key name)
    (declare (ignore client stream initargs name))
    nil))

(defmacro with-style ((client stream &rest initargs) &body body)
  (let ((previous-var (gensym)))
    `(let ((,previous-var (stream-style ,stream)))
       (setf (stream-style ,stream) (make-style ,client ,stream ,@initargs))
       (unwind-protect
            (progn ,@body)
         (setf (stream-style ,stream) ,previous-var)))))

(defgeneric stream-scale-column (stream column old-style new-style)
  (:method (stream column old-style new-style)
    (declare (ignore stream old-style new-style))
    column))

(defgeneric stream-measure-char (stream char &optional style)
  (:method (stream char &optional style)
    (declare (ignore stream char style))
    1))

(defgeneric stream-measure-string (stream string &optional start end style)
  (:method (stream string &optional start end style)
    (declare (ignore stream style))
    (- (or end (length string))
       (or start 0))))

(defmacro define-interface ((client-var client-class &optional intrinsic) &body body)
  (let* ((intrinsic-pkg (if intrinsic (find-package '#:common-lisp) *package*))
         (initial-pprint-dispatch-var (ensure-symbol '#:*initial-pprint-dispatch*))
         (standard-pprint-dispatch-var (ensure-symbol '#:*standard-pprint-dispatch*))
         (print-pprint-dispatch-var (ensure-symbol '#:*print-pprint-dispatch* intrinsic-pkg))
         (pprint-pop-func (ensure-symbol '#:pprint-pop intrinsic-pkg))
         (pprint-exit-if-list-exhausted-func (ensure-symbol '#:pprint-exit-if-list-exhausted intrinsic-pkg))
         (initialize-func (ensure-symbol '#:initialize-inravina)))
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
       (defun ,(ensure-symbol '#:pretty-stream-p) (stream)
         (pretty-stream-p ,client-var stream))
       (defun ,(ensure-symbol '#:copy-pprint-dispatch intrinsic-pkg) (&optional (table ,print-pprint-dispatch-var))
         #+ecl ,@(when intrinsic '((declare (ext:check-arguments-type nil))))
         (check-type table (or null dispatch-table))
         (copy-pprint-dispatch ,client-var (or table ,initial-pprint-dispatch-var)))
       (defun ,(ensure-symbol '#:set-pprint-dispatch intrinsic-pkg)
           (type-specifier function &optional (priority 0) (table ,print-pprint-dispatch-var))
         #+ecl ,@(when intrinsic '((declare (ext:check-arguments-type nil))))
         (check-type priority real)
         (check-type table dispatch-table)
         (check-type function (or symbol function))
         (set-pprint-dispatch ,client-var table type-specifier function priority))
       (defun ,(ensure-symbol '#:pprint-fill intrinsic-pkg) (stream object &optional (colon-p t) at-sign-p)
         (pprint-fill ,client-var (coerce-output-stream-designator stream)
                      object colon-p at-sign-p)
         nil)
       (defun ,(ensure-symbol '#:pprint-linear intrinsic-pkg) (stream object &optional (colon-p t) at-sign-p)
         (pprint-linear ,client-var (coerce-output-stream-designator stream)
                        object colon-p at-sign-p)
         nil)
       (defun ,(ensure-symbol '#:pprint-tabular intrinsic-pkg) (stream object &optional (colon-p t) at-sign-p (tabsize 16))
         (pprint-tabular ,client-var (coerce-output-stream-designator stream)
                         object colon-p at-sign-p tabsize)
         nil)
       (defun ,(ensure-symbol '#:pprint-indent intrinsic-pkg) (relative-to n &optional stream)
         (check-type relative-to (member :block :current))
         (pprint-indent ,client-var (coerce-output-stream-designator stream)
                        relative-to n)
         nil)
       (defun ,(ensure-symbol '#:pprint-newline intrinsic-pkg) (kind &optional stream)
         (check-type kind (member :linear :fill :miser :mandatory))
         (pprint-newline ,client-var (coerce-output-stream-designator stream)
                         kind)
         nil)
       (defun ,(ensure-symbol '#:pprint-tab intrinsic-pkg) (kind colnum colinc &optional stream)
         (check-type kind (member :line :section :line-relative :section-relative))
         (pprint-tab ,client-var (coerce-output-stream-designator stream)
                     kind colnum colinc)
         nil)
       (defun ,(ensure-symbol '#:pprint-array) (stream object &optional colon-p at-sign-p)
         (pprint-array ,client-var (coerce-output-stream-designator stream)
                       object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-apply) (stream object &optional colon-p at-sign-p)
         (pprint-apply ,client-var (coerce-output-stream-designator stream)
                       object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-bindings) (stream object &optional colon-p at-sign-p)
         (pprint-bindings ,client-var (coerce-output-stream-designator stream)
                          object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-call) (stream object &optional colon-p at-sign-p)
         (pprint-call ,client-var (coerce-output-stream-designator stream)
                      object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-case) (stream object &optional colon-p at-sign-p)
         (pprint-case ,client-var (coerce-output-stream-designator stream)
                      object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-cond) (stream object &optional colon-p at-sign-p)
         (pprint-cond ,client-var (coerce-output-stream-designator stream)
                      object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-defclass) (stream object &optional colon-p at-sign-p)
         (pprint-defclass ,client-var (coerce-output-stream-designator stream)
                          object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-defun) (stream object &optional colon-p at-sign-p)
         (pprint-defun ,client-var (coerce-output-stream-designator stream)
                       object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-defmethod) (stream object &optional colon-p at-sign-p)
         (pprint-defmethod ,client-var (coerce-output-stream-designator stream)
                           object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-defpackage) (stream object &optional colon-p at-sign-p)
         (pprint-defpackage ,client-var (coerce-output-stream-designator stream)
                            object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-do) (stream object &optional colon-p at-sign-p)
         (pprint-do ,client-var (coerce-output-stream-designator stream)
                    object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-dolist) (stream object &optional colon-p at-sign-p)
         (pprint-dolist ,client-var (coerce-output-stream-designator stream)
                        object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-flet) (stream object &optional colon-p at-sign-p)
         (pprint-flet ,client-var (coerce-output-stream-designator stream)
                      object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-lambda) (stream object &optional colon-p at-sign-p)
         (pprint-lambda ,client-var (coerce-output-stream-designator stream)
                        object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-let) (stream object &optional colon-p at-sign-p)
         (pprint-let ,client-var (coerce-output-stream-designator stream)
                     object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-loop) (stream object &optional colon-p at-sign-p)
         (pprint-loop ,client-var (coerce-output-stream-designator stream)
                      object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-multiple-value-bind) (stream object &optional colon-p at-sign-p)
         (pprint-multiple-value-bind ,client-var (coerce-output-stream-designator stream)
                                     object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-prog) (stream object &optional colon-p at-sign-p)
         (pprint-prog ,client-var (coerce-output-stream-designator stream)
                      object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-prog1) (stream object &optional colon-p at-sign-p)
         (pprint-prog1 ,client-var (coerce-output-stream-designator stream)
                       object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-prog2) (stream object &optional colon-p at-sign-p)
         (pprint-prog2 ,client-var (coerce-output-stream-designator stream)
                       object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-progn) (stream object &optional colon-p at-sign-p)
         (pprint-progn ,client-var (coerce-output-stream-designator stream)
                       object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-progv) (stream object &optional colon-p at-sign-p)
         (pprint-progv ,client-var (coerce-output-stream-designator stream)
                       object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-symbol-macrolet) (stream object &optional colon-p at-sign-p)
         (pprint-symbol-macrolet ,client-var (coerce-output-stream-designator stream)
                                 object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-tagbody) (stream object &optional colon-p at-sign-p)
         (pprint-tagbody ,client-var (coerce-output-stream-designator stream)
                         object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-function-call) (stream object &optional colon-p at-sign-p argument-count)
         (pprint-function-call ,client-var (coerce-output-stream-designator stream)
                               object colon-p at-sign-p argument-count))
       (defun ,(ensure-symbol '#:pprint-argument-list) (stream object &optional colon-p at-sign-p argument-count)
         (pprint-argument-list ,client-var (coerce-output-stream-designator stream)
                               object colon-p at-sign-p argument-count))
       (defun ,(ensure-symbol '#:pprint-with) (stream object &optional colon-p at-sign-p argument-count)
         (pprint-with ,client-var (coerce-output-stream-designator stream)
                      object colon-p at-sign-p argument-count))
       (defun ,(ensure-symbol '#:pprint-lambda-list) (stream object &optional colon-p at-sign-p)
         (pprint-lambda-list ,client-var (coerce-output-stream-designator stream)
                             object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-destructuring-bind) (stream object &optional colon-p at-sign-p)
         (pprint-destructuring-bind ,client-var (coerce-output-stream-designator stream)
                                    object colon-p at-sign-p))
       (defun ,(ensure-symbol '#:pprint-macro-char) (stream object &optional colon-p at-sign-p disp-char sub-char)
         (pprint-macro-char ,client-var (coerce-output-stream-designator stream)
                            object colon-p at-sign-p disp-char sub-char))
       (defun ,(ensure-symbol '#:pprint-dispatch intrinsic-pkg) (object &optional (table ,print-pprint-dispatch-var))
         #+ecl ,@(when intrinsic '((declare (ext:check-arguments-type nil))))
         (check-type table (or null dispatch-table))
         (pprint-dispatch ,client-var (or table ,initial-pprint-dispatch-var) object))
       (defmacro ,(ensure-symbol '#:pprint-logical-block intrinsic-pkg) ((stream-symbol object
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
         (setf ,initial-pprint-dispatch-var (copy-pprint-dispatch ,client-var nil t)
               ,standard-pprint-dispatch-var (copy-pprint-dispatch ,client-var nil t)
               ,print-pprint-dispatch-var (copy-pprint-dispatch ,client-var nil))
         ,@body))))

(defgeneric execute-logical-block (client stream object function
                                   &key prefix per-line-prefix-p suffix))
