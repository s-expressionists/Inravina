(in-package #:inravina)

(defclass client ()
  ())

(defvar *client* (make-instance 'client))

(defmacro pprint-logical-block ((stream-symbol object &key
                                               (prefix nil prefix-p)
                                               (per-line-prefix nil per-line-prefix-p)
                                               (suffix ""))
                                &body body)
  (when (and prefix-p per-line-prefix-p)
    (error 'program-error))
  (let ((tag-name (gensym))
        (object-var (gensym)))
    `(let ((,stream-symbol (make-pretty-stream *client* ,stream-symbol))
           (,object-var ,object))
       (unwind-protect
           (block ,tag-name
             (pprint-start-logical-block *client* ,stream-symbol
                                         ,(if (or prefix-p per-line-prefix-p)
                                            prefix
                                            "")
                                         ,per-line-prefix ,suffix)
             (macrolet ((pprint-exit-if-list-exhausted ()
                          (unless ,object-var
                            (return-from ,tag-name)))
                        (pprint-pop ()
                          (pop ,object-var)))
               ,@body))
         (pprint-end-logical-block *client* ,stream-symbol)))))

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

