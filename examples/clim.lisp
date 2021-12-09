; Once this script is loaded you can call incless:pprint from the Listener REPL.

(asdf:load-system "clim-listener")
(asdf:load-system :inravina/ext.extrinsic)

(in-package :clim-user)

(defstruct style
  ink
  text-style)

(defun em-size (stream style)
  (clim:stream-character-width stream #\M
                               :text-style (and style (style-text-style style))))

(defmethod trivial-stream-column:stream-measure-string
    ((stream clim:standard-extended-output-stream) string &optional start end style)
  (/ (clim:stream-string-width stream string
                               :start start :end end
                               :text-style (and style (style-text-style style)))
     (em-size stream style)))

(defmethod trivial-stream-column:stream-measure-char
    ((stream clim:standard-extended-output-stream) char &optional style)
  (/ (clim:stream-character-width stream char
                                  :text-style (and style (style-text-style style)))
     (em-size stream style)))

(defmethod trivial-gray-streams:stream-advance-to-column
    ((stream clim:standard-extended-output-stream) column)
  (setf (clim:stream-cursor-position stream)
        (values (* (em-size stream nil)
                   column)
                (nth-value 1 (clim:stream-cursor-position stream))))
  column)

(defmethod trivial-stream-column:stream-style ((stream clim:standard-extended-output-stream))
  (make-style :ink (clim:medium-ink stream) :text-style (clim:medium-text-style stream)))

(defmethod (setf trivial-stream-column:stream-style)
    (new-style (stream clim:standard-extended-output-stream))
  (setf (clim:medium-text-style stream) (style-text-style new-style)
        (clim:medium-ink stream) (style-ink new-style)))

(defmethod trivial-stream-column:stream-copy-style
    ((stream clim:standard-extended-output-stream) style
     &rest overrides
     &key ink family face size &allow-other-keys)
  (declare (ignore overrides))
  (make-style :ink (cond (ink)
                         (style (style-ink style))
                         (t (clim:medium-ink stream)))
              :text-style (clim:merge-text-styles (clim:make-text-style family face size)
                                                  (if style
                                                      (style-text-style style)
                                                      (clim:medium-text-style stream)))))

(defmethod trivial-stream-column:stream-scale-column
    ((stream clim:standard-extended-output-stream) column old-style new-style)
  (/ (* column (em-size stream old-style))
     (em-size stream new-style)))

(defmacro call-next-method-with-styles (stream overrides)
  (let ((overrides-var (gensym))
        (old-style-var (gensym)))
    `(let ((,overrides-var ,overrides))
       (if ,overrides-var
           (let ((,old-style-var (trivial-stream-column:stream-style ,stream)))
             (setf (trivial-stream-column:stream-style ,stream)
                   (apply #'trivial-stream-column:stream-copy-style ,stream nil ,overrides-var))
             (unwind-protect
                 (call-next-method)
               (setf (trivial-stream-column:stream-style ,stream) ,old-style-var)))
           (call-next-method)))))

(defmethod incless:print-object-using-client :around (client (sym symbol) stream)
  (call-next-method-with-styles stream
    (cond ((keywordp sym)
           (list :ink clim:+red3+))
          ((or (special-operator-p sym)
               (macro-function sym))
           (list :ink clim:+steel-blue+ :face :bold))
          ((constantp sym)
           (list :ink clim:+darkgoldenrod4+ :face :italic))
          ((boundp sym)
           (list :ink clim:+darkgoldenrod4+)))))

(defmethod incless:print-object-using-client :around (client (object number) stream)
  (call-next-method-with-styles stream (list :ink clim:+cadet-blue+)))

(defmethod incless:print-object-using-client :around (client (object string) stream)
  (call-next-method-with-styles stream (list :ink clim:+green4+)))

(clim-listener:run-listener)

