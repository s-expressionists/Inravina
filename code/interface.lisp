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

(defgeneric make-dispatch-function (client pattern function rest)
  (:method (client (pattern (eql :stream-object)) function (rest null))
    (declare (ignore client))
    function)
  (:method (client (pattern (eql :stream-object)) function rest)
    (declare (ignore client))
    (lambda (stream object)
      (apply function stream object rest)))
  (:method (client (pattern (eql :object-stream)) function rest)
    (declare (ignore client))
    (lambda (stream object)
      (apply function object stream rest))))

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

(defgeneric execute-logical-block (client stream object function
                                   &key prefix per-line-prefix-p suffix))

(trinsic:make-define-interface (:client-form client-form :client-class client-class :intrinsic intrinsicp)
    ((copy-pprint-dispatch-sym cl:copy-pprint-dispatch)
     (initial-pprint-dispatch-sym #:*initial-pprint-dispatch*)
     (pprint-dispatch-sym cl:pprint-dispatch)
     (pprint-exit-if-list-exhausted-sym cl:pprint-exit-if-list-exhausted)
     (pprint-fill-sym cl:pprint-fill)
     (pprint-indent-sym cl:pprint-indent)
     (pprint-linear-sym cl:pprint-linear)
     (pprint-logical-block-sym cl:pprint-logical-block)
     (pprint-newline-sym cl:pprint-newline)
     (pprint-pop-sym cl:pprint-pop)
     (pprint-tab-sym cl:pprint-tab)
     (pprint-tabular-sym cl:pprint-tabular)
     (pretty-stream-p-sym #:pretty-stream-p)
     (print-pprint-dispatch-sym cl:*print-pprint-dispatch*)
     (set-pprint-dispatch-sym cl:set-pprint-dispatch)
     (standard-pprint-dispatch-sym #:*standard-pprint-dispatch*))
   `((defmethod make-dispatch-function
         ((client ,client-class) (pattern (eql :client-stream-object)) function rest)
       (lambda (stream object)
         (apply function ,client-form stream object rest)))

     (defmethod make-dispatch-function
         ((client ,client-class) (pattern (eql :client-object-stream)) function rest)
       (lambda (stream object)
         (apply function ,client-form object stream rest)))

     (defvar ,initial-pprint-dispatch-sym (copy-pprint-dispatch ,client-form nil t))

     (defvar ,standard-pprint-dispatch-sym (copy-pprint-dispatch ,client-form :standard t))

     (defvar ,print-pprint-dispatch-sym (copy-pprint-dispatch ,client-form nil))

     (defun ,pretty-stream-p-sym (stream)
       (pretty-stream-p ,client-form stream))

     (defun ,copy-pprint-dispatch-sym (&optional (table ,print-pprint-dispatch-sym))
       #+ecl ,@(when intrinsicp '((declare (ext:check-arguments-type nil))))
       (check-type table (or null dispatch-table))
       (copy-pprint-dispatch ,client-form (or table ,initial-pprint-dispatch-sym)))

     (defun ,set-pprint-dispatch-sym
         (type-specifier function &optional (priority 0) (table ,print-pprint-dispatch-sym))
       #+ecl ,@(when intrinsicp '((declare (ext:check-arguments-type nil))))
       (check-type priority real)
       (check-type table dispatch-table)
       (check-type function (or symbol function))
       (set-pprint-dispatch ,client-form table type-specifier function priority))

     (defun ,pprint-fill-sym (stream object &optional (colon-p t) at-sign-p)
       (pprint-fill ,client-form (coerce-output-stream-designator stream)
                    object colon-p at-sign-p)
       nil)

     (defun ,pprint-linear-sym (stream object &optional (colon-p t) at-sign-p)
       (pprint-linear ,client-form (coerce-output-stream-designator stream)
                      object colon-p at-sign-p)
       nil)

     (defun ,pprint-tabular-sym (stream object &optional (colon-p t) at-sign-p (tabsize 16))
       (pprint-tabular ,client-form (coerce-output-stream-designator stream)
                       object colon-p at-sign-p tabsize)
       nil)

     (defun ,pprint-indent-sym (relative-to n &optional stream)
       (check-type relative-to (member :block :current))
       (pprint-indent ,client-form (coerce-output-stream-designator stream)
                      relative-to n)
       nil)

     (defun ,pprint-newline-sym (kind &optional stream)
       (check-type kind (member :linear :fill :miser :mandatory))
       (pprint-newline ,client-form (coerce-output-stream-designator stream)
                       kind)
       nil)

     (defun ,pprint-tab-sym (kind colnum colinc &optional stream)
       (check-type kind (member :line :section :line-relative :section-relative))
       (pprint-tab ,client-form (coerce-output-stream-designator stream)
                   kind colnum colinc)
       nil)

     (defun ,pprint-dispatch-sym (object &optional (table ,print-pprint-dispatch-sym))
       #+ecl ,@(when intrinsicp '((declare (ext:check-arguments-type nil))))
       (check-type table (or null dispatch-table))
       (pprint-dispatch ,client-form (or table ,initial-pprint-dispatch-sym) object))

     (defmacro ,pprint-logical-block-sym ((stream-symbol object
                                           &key (prefix "" prefix-p)
                                                (per-line-prefix "" per-line-prefix-p)
                                                (suffix "" suffix-p))
                                          &body body)
       (expand-logical-block ',client-form stream-symbol object
                             prefix prefix-p per-line-prefix per-line-prefix-p suffix suffix-p
                             ',pprint-exit-if-list-exhausted-sym ',pprint-pop-sym
                             body))

     (defmacro ,pprint-exit-if-list-exhausted-sym ()
       "Tests whether or not the list passed to the lexically current logical block has
 been exhausted. If this list has been reduced to nil, pprint-exit-if-list-exhausted
 terminates the execution of the lexically current logical block except for the
 printing of the suffix. Otherwise pprint-exit-if-list-exhausted returns nil."
       (error "PPRINT-EXIT-IF-LIST-EXHAUSTED must be lexically inside PPRINT-LOGICAL-BLOCK."))

     (defmacro ,pprint-pop-sym ()
       "Pops one element from the list being printed in the lexically current logical
 block, obeying *print-length* and *print-circle*."
       (error "PPRINT-POP must be lexically inside PPRINT-LOGICAL-BLOCK."))))
