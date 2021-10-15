(in-package #:inravina)

(deftype newline-kind ()
  `(member :fill :linear :mandatory :miser
           :literal-fill :literal-linear :literal-mandatory :literal-miser))

(defun fill-kind-p (kind)
  (and (member kind '(:fill :literal-fill))
       t))

(defun linear-kind-p (kind)
  (and (member kind '(:linear :literal-linear))
       t))

(defun mandatory-kind-p (kind)
  (and (member kind '(:mandatory :literal-mandatory))
       t))

(defun miser-kind-p (kind)
  (and (member kind '(:miser :literal-miser))
       t))

(defun literal-kind-p (kind)
  (and (member kind '(:literal-fill :literal-linear :literal-mandatory :literal-miser))
       t))

(defmacro pprint-logical-block ((client stream-symbol object &key
                                 (prefix nil prefix-p)
                                 (per-line-prefix nil per-line-prefix-p)
                                 (suffix ""))
                                &body body)
  (when (and prefix-p per-line-prefix-p)
    (error 'program-error))
  (check-type stream-symbol symbol)
  (let ((tag-name (make-symbol "L"))
        (object-var (make-symbol "T"))
        (stream-var (cond
                      ((null stream-symbol)
                        '*standard-output*)
                      ((eq t stream-symbol)
                        '*terminal-io*)
                      (t
                        stream-symbol))))
    `(let* ((*client* ,client)
            (,stream-var (make-pretty-stream *client* ,stream-symbol))
            (,object-var ,object))
       (declare (ignorable ,stream-var ,object-var))
       (unwind-protect
           (block ,tag-name
             (pprint-start-logical-block *client* ,stream-var
                                         ,(if (or prefix-p per-line-prefix-p)
                                            prefix
                                            "")
                                         ,per-line-prefix)
             (macrolet ((pprint-exit-if-list-exhausted ()
                          '(unless ,object-var
                             (return-from ,tag-name)))
                        (pprint-pop ()
                          '(pop ,object-var)))
               ,@body))
         (pprint-end-logical-block *client* ,stream-var ,suffix)))))

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

(defmacro pprint-tail (client stream kind)
  `(loop (pprint-exit-if-list-exhausted)
         (write-char #\Space ,stream)
         (pprint-newline ,client ,kind ,stream)
         (write (pprint-pop) :stream ,stream)))

(defmacro pprint-format-logical-block ((client stream object colon-p at-sign-p) &body body)
  `(pprint-logical-block (,client ,stream ,object :prefix (if ,colon-p "(" "")
                                                  :suffix (if ,colon-p ")" ""))
     ,@body))

(defmacro pprint-body-form ((client stream object colon-p at-sign-p) &body body)
  `(pprint-format-logical-block (,client ,stream ,object ,colon-p ,at-sign-p)
     ,@body
     (pprint-exit-if-list-exhausted)
     (pprint-indent ,client :block 1 ,stream)
     (pprint-newline ,client :mandatory ,stream)
     (loop (write (pprint-pop) :stream ,stream)
           (pprint-exit-if-list-exhausted)
           (write-char #\Space ,stream)
           (pprint-newline ,client :linear ,stream))))

(defmacro pprint-tagbody-form ((client stream object colon-p at-sign-p) &body body)
  `(pprint-format-logical-block (,client ,stream ,object ,colon-p ,at-sign-p)
     ,@body
     (pprint-exit-if-list-exhausted)
     (pprint-newline ,client :mandatory ,stream)
     (loop for form-or-tag = (pprint-pop)
           do (pprint-indent ,client :block
                             (if (atom form-or-tag) 0 1)
                             ,stream)
           do (write form-or-tag :stream ,stream)
           do (pprint-exit-if-list-exhausted)
           do (write-char #\Space ,stream)
           do (pprint-newline ,client :linear ,stream))))

(defmethod pprint-fill (client stream object &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (pprint-logical-block (client stream object :prefix (if colon-p "(" "")
                                              :suffix (if colon-p ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write (pprint-pop) :stream stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\space stream)
          (pprint-newline client :fill stream))))

(defmethod pprint-linear (client stream object &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (pprint-logical-block (client stream object :prefix (if colon-p "(" "")
                                              :suffix (if colon-p ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write (pprint-pop) :stream stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-newline client :linear stream))))

(defmethod pprint-tabular (client stream object &optional colon-p at-sign-p tabsize)
  (declare (ignore at-sign-p))
  (unless tabsize
    (setq tabsize 16))
  (pprint-logical-block (client stream object :prefix (if colon-p "(" "")
                                              :suffix (if colon-p ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write (pprint-pop) :stream stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\space stream)
          (pprint-tab client :section-relative 0 tabsize stream)
          (pprint-newline client :fill stream))))

(defmethod text-width (client stream text &optional start end)
  (- (or end (length text))
     (or start 0)))

(defmethod break-position (client stream text)
  (1+ (or (position-if (lambda (ch)
                     (char/= ch #\Space))
                   text :from-end t)
      -1)))

(defmethod normalize-text (client stream text)
  text)

(defmethod arrange-text (client stream (text (eql nil)))
  (values nil 0 0))

(defmethod right-margin (client stream)
  (or *print-right-margin* 100))

(defmethod pprint-split (client stream text &optional start end)
  (prog (pos)
   next
    (setf pos (position #\newline text :start (or start 0) :end end))
    (when pos
      (pprint-text client stream text start pos)
      (pprint-newline client :literal-mandatory stream)
      (setf start (1+ pos))
      (go next))
    (pprint-text client stream text start end)))

(defmethod pprint-bindings (client stream object &optional colon-p at-sign-p)
  (pprint-logical-block (client stream object :prefix (if colon-p "(" "")
                                              :suffix (if colon-p ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (pprint-linear client stream (pprint-pop) colon-p at-sign-p)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-newline client :linear stream))))

(defmethod pprint-block (client stream object &optional colon-p at-sign-p)
  (pprint-body-form (client stream object colon-p at-sign-p)
    (pprint-exit-if-list-exhausted)
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (pprint-indent client :block 3 stream)
    (write-char #\Space stream)
    (pprint-newline client :fill stream)
    (write (pprint-pop) :stream stream)))

(defmethod pprint-do (client stream object &optional colon-p at-sign-p)
  (pprint-tagbody-form (client stream object colon-p at-sign-p)
    (pprint-exit-if-list-exhausted)
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-indent client :current 0 stream)
    (pprint-newline client :fill stream)
    (pprint-bindings client stream (pprint-pop) colon-p at-sign-p)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-newline client :fill stream)
    (pprint-bindings client stream (pprint-pop) colon-p at-sign-p)))

(defmethod pprint-let (client stream object &optional colon-p at-sign-p)
  (pprint-body-form (client stream object colon-p at-sign-p)
    (pprint-exit-if-list-exhausted)
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-indent client :current 0 stream)
    (pprint-newline client :miser stream)
    (pprint-bindings client stream (pprint-pop) colon-p at-sign-p)))

#+(or)(defun wibble ()
  (let ((stream (make-pretty-stream *client* nil)))
    (pprint-start-logical-block *client* stream "(" nil)
    (write-string " " stream)
    (pprint-newline *client* :fill stream)
    (write-string "bc " stream)
    (pprint-start-logical-block *client* stream "(" nil)
    (write-string "d " stream)
    (pprint-start-logical-block *client* stream "(" nil)
    (write-string "e " stream)
    (pprint-newline *client* :fill stream)
    (write-string "fg " stream)
    (pprint-newline *client* :fill stream)
    (write-string " " stream)
    (pprint-end-logical-block *client* stream ")")
    (write-string "h " stream)
    (pprint-newline *client* :fill stream)
    (write-string "j " stream)
    (pprint-end-logical-block *client* stream ")")
    (write-string "k" stream)
    (pprint-end-logical-block *client* stream ")")))

