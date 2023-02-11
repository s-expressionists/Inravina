(in-package #:inravina)

(defun pprint-valid-list-p (client stream object)
  (declare (ignore client))
  (and (listp object)
       (not (and (getf *quasiquote* stream)
                 (typep object 'unquote-form)))))

(defun pprint-pop-p (client stream object count)
  (cond ((and (plusp count)
              (not (pprint-valid-list-p client stream object)))
         (write-string ". " stream)
         (incless:write-object client object stream)
         nil)
        ((and (not *print-readably*)
              (eql count *print-length*))
         (write-string "..." stream)
         nil)
        ((and *print-circle*
              object
              (plusp count)
              (incless:circle-check client object))
         (write-string ". " stream)
         (incless:write-object client object stream)
         nil)
        (t)))

(defun expand-logical-block (client stream-symbol object
                             prefix prefix-p per-line-prefix per-line-prefix-p suffix suffix-p
                             pprint-exit-if-list-exhausted pprint-pop
                             body)
  (when (and prefix-p per-line-prefix-p)
    (error 'program-error))
  (check-type stream-symbol symbol)
  (let ((block-name (gensym))
        (object-var (gensym))
        (count-var (gensym))
        (stream-var (cond ((null stream-symbol)
                           '*standard-output*)
                          ((eq t stream-symbol)
                           '*terminal-io*)
                          (t
                           stream-symbol)))
        (prefix-var (gensym))
        (suffix-var (gensym)))
    `(let ((,stream-var (make-pretty-stream ,client (coerce-output-stream-designator ,stream-symbol)))
           (,object-var ,object)
           (,prefix-var ,(cond (per-line-prefix-p per-line-prefix)
                               (prefix-p prefix)
                               (t "")))
           (,suffix-var ,suffix))
       (check-type ,prefix-var string)
       (check-type ,suffix-var string)
       (cond ((not (pprint-valid-list-p ,client ,stream-var ,object-var))
              (incless:write-object ,client ,object-var ,stream-var))
             ((and (not *print-readably*)
                   (eql 0 *print-level*))
              (write-char #\# ,stream-var))
             (t
              (incless:handle-circle ,client ,object-var ,stream-var
                                     (lambda (,object-var ,stream-var
                                              &aux (*print-level* (and *print-level* (max 0 (1- *print-level*))))
                                                   (,count-var 0))
                                       (declare (ignorable ,stream-var ,object-var ,count-var))
                                       (pprint-start-logical-block ,client ,stream-var ,prefix-var ,per-line-prefix-p)
                                       (unwind-protect
                                            (block ,block-name
                                              (macrolet ((,pprint-exit-if-list-exhausted ()
                                                           '(unless ,object-var
                                                             (return-from ,block-name)))
                                                         (,pprint-pop ()
                                                           '(progn
                                                             (unless (pprint-pop-p ,client ,stream-var ,object-var ,count-var)
                                                               (return-from ,block-name))
                                                             (incf ,count-var)
                                                             (pop ,object-var))))
                                                ,@body))
                                         (pprint-end-logical-block ,client ,stream-var ,suffix-var))))))
       nil)))

(defmacro pprint-logical-block ((client stream-symbol object
                                &key (prefix "" prefix-p)
                                     (per-line-prefix "" per-line-prefix-p)
                                     (suffix "" suffix-p))
                                &body body)
  (expand-logical-block client stream-symbol object
                        prefix prefix-p per-line-prefix per-line-prefix-p suffix suffix-p
                        'pprint-exit-if-list-exhausted 'pprint-pop
                        body))

(defmacro pprint-exit-if-list-exhausted ()
  "Tests whether or not the list passed to the lexically current logical block has
   been exhausted. If this list has been reduced to nil, pprint-exit-if-list-exhausted
   terminates the execution of the lexically current logical block except for the
   printing of the suffix. Otherwise pprint-exit-if-list-exhausted returns nil."
  (error "PPRINT-EXIT-IF-LIST-EXHAUSTED must be lexically inside PPRINT-LOGICAL-BLOCK."))

(defmacro pprint-pop ()
  "Pops one element from the list being printed in the lexically current logical
   block, obeying *print-length* and *print-circle*."
  (error "PPRINT-POP must be lexically inside PPRINT-LOGICAL-BLOCK."))

(defmacro pprint-logical-sub-block ((client stream
                                     &key (prefix nil prefix-p)
                                          (per-line-prefix nil per-line-prefix-p)
                                          (suffix ""))
                                    &body body)
  (when (and prefix-p per-line-prefix-p)
    (error 'program-error))
  `(unwind-protect
       (progn (pprint-start-logical-block ,client ,stream ,prefix ,per-line-prefix)
              ,@body)
     (pprint-end-logical-block ,client ,stream ,suffix)))
