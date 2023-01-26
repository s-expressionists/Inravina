(in-package #:inravina)

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

(defgeneric copy-pprint-dispatch (client table &optional read-only))

(defgeneric pprint-dispatch (client table object)
  (:method (client table object)
    (declare (ignore client table object))
    (values (lambda (stream object)
              (print-object object stream))
            nil)))

(defgeneric set-pprint-dispatch (client table type-specifier function &optional priority pattern arguments))

(defgeneric make-pprint-dispatch-iterator (client table))

(defgeneric pprint-fill (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-linear (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-tabular (client stream object &optional colon-p at-sign-p tabsize))

(defgeneric pprint-indent (client stream relative-to n)
  (:method (client stream relative-to n)
    (declare (ignore client stream relative-to n))))

(defgeneric pprint-newline (client stream kind)
  (:method (client stream kind)
    (declare (ignore client stream kind))))

(defgeneric pprint-tab (client stream kind colnum colinc)
  (:method (client stream kind colnum colinc)
    (declare (ignore client stream kind colnum colinc))))

(defgeneric pprint-split (client stream text &optional start end))

(defgeneric pprint-text (client stream text &optional start end))

(defgeneric pprint-fill-plist (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-linear-plist (client stream object &optional colon-p at-sign-p))

(defgeneric pprint-tabular-plist (client stream object &optional colon-p at-sign-p tabsize))

(defgeneric pprint-start-logical-block (client stream prefix per-line-prefix-p)
  (:method (client stream prefix per-line-prefix-p)
    (declare (ignore client stream prefix per-line-prefix-p))))

(defgeneric pprint-end-logical-block (client stream suffix)
  (:method (client stream suffix)
    (declare (ignore client stream suffix))))

(defgeneric make-pretty-stream (client stream))

(defgeneric pretty-stream-p (stream)
  (:method (stream)
    (declare (ignore stream))
    nil))

(defgeneric break-position (client stream text))

(defgeneric normalize-text (client stream text))

(defgeneric pprint-prog1 (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-defun (client stream object &rest options &key &allow-other-keys))

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

(defgeneric pprint-sbcl-comma (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-call (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-apply (client stream object &rest options &key &allow-other-keys))

(defgeneric pprint-defclass (client stream object &rest options &key &allow-other-keys))

(defmacro frob-output-stream (stream)
  (let ((svar (gensym)))
    `(let ((,svar ,stream))
       (cond ((null ,svar) 
              *standard-output*)
             ((eq ,svar t) 
              *terminal-io*)
             ((output-stream-p ,svar)
              ,svar)
             (t
              (error 'simple-type-error
                     :datum ,svar
                     :expected-type '(satisfies output-stream-p)
                     :format-control "~S isn't an output stream."
                     :format-arguments (list ,svar)))))))

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

(defgeneric make-dispatch-function (client pattern function rest))
