(asdf:load-system :inravina/ext.extrinsic)

(defclass term-pretty-stream (inravina:pretty-stream)
  ())

(defclass term-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((style :accessor style
          :initform (list :color :default)
          :type list)
   (target :reader target
           :initarg :target)))

(defmethod inravina:make-pretty-stream ((client inravina:client) (stream term-stream))
  (make-instance 'term-pretty-stream :target stream :client client))

;;; Gray Stream protocol support

(defmethod trivial-gray-streams:stream-file-position ((stream term-stream))
  (file-position (target stream)))

(defmethod trivial-gray-streams:stream-write-char ((stream term-stream) char)
  (write-char char (target stream)))

(defmethod trivial-gray-streams:stream-write-string ((stream term-stream) string &optional start end)
  (write-string string (target stream) :start (or start 0) :end end))

(defmethod trivial-gray-streams:stream-finish-output ((stream term-stream))
  (finish-output (target stream)))

(defmethod trivial-gray-streams:stream-force-output ((stream term-stream))
  (force-output (target stream)))

(defmethod trivial-gray-streams:stream-clear-output ((stream term-stream))
  (clear-output (target stream)))

(defmethod trivial-gray-streams:stream-terpri ((stream term-stream))
  (terpri (target stream)))

(defmethod trivial-gray-streams:stream-fresh-line ((stream term-stream))
  (fresh-line (target stream)))

(defmethod trivial-gray-streams:stream-line-column ((stream term-stream))
  (trivial-stream-column:line-column (target stream)))

(defmethod trivial-gray-streams:stream-advance-to-column ((stream term-stream) column)
  (trivial-stream-column:advance-to-column column (target stream)))

;;; trivial-stream-column protocal support

(defmethod trivial-stream-column:stream-style ((stream term-stream))
  (style stream))

(defvar +colors+
  '(:default 39 :black 30 :red 31 :green 32 :yellow 33 :blue 34 :magenta 35 :cyan 36
    :white 37 :bright-black 90 :bright-red 91 :bright-green 92 :bright-yellow 93
    :bright-blue 94 :bright-magenta 95 :bright-cyan 96 :bright-white 97))

(defmethod (setf trivial-stream-column:stream-style) (new-style (stream term-stream))
  (setf (style stream) new-style)
  (let ((color (getf +colors+ (getf new-style :color))))
    (when color
      (format (target stream) "~C[~2,0Dm" #\Escape color))))

(defmethod trivial-stream-column:stream-copy-style ((stream term-stream) style &rest overrides &key &allow-other-keys)
  (append overrides (or style (style stream))))

(defmacro call-next-method-with-styles (stream overrides)
  (let ((overrides-var (gensym))
        (old-style-var (gensym)))
    `(let ((,overrides-var ,overrides))
       (if (and *print-pretty* ,overrides-var)
           (let ((,old-style-var (trivial-stream-column:stream-style ,stream)))
             (setf (trivial-stream-column:stream-style ,stream)
                   (apply #'trivial-stream-column:stream-copy-style ,stream nil ,overrides-var))
             (unwind-protect
                 (call-next-method)
               (setf (trivial-stream-column:stream-style ,stream) ,old-style-var)))
           (call-next-method)))))

(defmethod incless:print-object-using-client :around
    (client (sym symbol) (stream term-pretty-stream))
  (call-next-method-with-styles stream
    (cond ((keywordp sym)
           (list :color :red))
          ((or (special-operator-p sym)
               (macro-function sym))
           (list :color :blue))
          ((constantp sym)
           (list :color :cyan))
          ((boundp sym)
           (list :color :yellow)))))

(defmethod incless:print-object-using-client :around
    (client (object number) (stream term-pretty-stream))
  (call-next-method-with-styles stream (list :color :green)))

(defmethod incless:print-object-using-client :around
    (client (object string) (stream term-pretty-stream))
  (call-next-method-with-styles stream (list :color :green)))

(setf *standard-output* (make-instance 'term-stream :target *standard-output*))
