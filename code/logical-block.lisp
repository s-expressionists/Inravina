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
         (incless/core:write-object client object stream)
         nil)
        ((and (not *print-readably*)
              (eql count *print-length*))
         (write-string "..." stream)
         nil)
        ((and *print-circle*
              object
              (plusp count)
              (incless/core:circle-check client object))
         (write-string ". " stream)
         (incless/core:write-object client object stream)
         nil)
        (t)))

(defun do-pprint-logical-block (client stream object prefix per-line-prefix-p suffix function)
  (check-type prefix string)
  (check-type suffix string)
  (let ((*client* client)
        (stream (make-pretty-stream client
                                    (cond ((null stream)
                                           *standard-output*)
                                          ((eq t stream)
                                           *terminal-io*)
                                          (t
                                           stream)))))
    (cond ((not (pprint-valid-list-p client stream object))
           (incless/core:write-object client object stream))
          ((and (not *print-readably*)
                (eql 0 *print-level*))
           (write-char #\# stream))
          (t
           (incless/core:handle-circle client object stream
                                       (lambda (object stream)
                                         (let ((*print-level* (and *print-level* (max 0 (1- *print-level*)))))
                                           (pprint-start-logical-block client stream prefix per-line-prefix-p)
                                           (unwind-protect
                                                (funcall function stream object)
                                             (pprint-end-logical-block client stream suffix)))))))
    nil))

(defmacro pprint-logical-block ((client stream-symbol object
                                &key (prefix "" prefix-p)
                                     (per-line-prefix "" per-line-prefix-p)
                                     (suffix ""))
                                &body body)
  (when (and prefix-p per-line-prefix-p)
    (error 'program-error))
  (check-type stream-symbol symbol)
  (let ((tag-name (gensym))
        (object-var (gensym))
        (count-var (gensym))
        (stream-var (cond ((null stream-symbol)
                           '*standard-output*)
                          ((eq t stream-symbol)
                           '*terminal-io*)
                          (t
                           stream-symbol))))
    `(do-pprint-logical-block ,client ,stream-symbol ,object
                              ,(if per-line-prefix-p
                                   per-line-prefix
                                   prefix)
                              ,per-line-prefix-p ,suffix
                              (lambda (,stream-var ,object-var &aux (,count-var 0))
                                (declare (ignorable ,stream-var ,object-var))
                                (block ,tag-name
                                  (macrolet ((pprint-exit-if-list-exhausted ()
                                               '(unless ,object-var
                                                  (return-from ,tag-name)))
                                             (pprint-pop ()
                                               '(progn
                                                  (unless (pprint-pop-p ,client ,stream-var ,object-var ,count-var)
                                                    (return-from ,tag-name))
                                                  (incf ,count-var)
                                                  (pop ,object-var))))
                                    ,@body))))))

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
