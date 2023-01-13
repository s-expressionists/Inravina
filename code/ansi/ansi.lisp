(in-package #:inravina-ansi)

(deftype color ()
  `(member :default :black :red :green :yellow :blue :magenta :cyan :white))

(deftype font ()
  `(member :default :bold :faint :italic :underline :slow-blink :rapid-blink :invert))

(defclass ansi-style ()
  ((foreground :accessor foreground
               :initarg :foreground
               :initform :default
               :type color)
   (background :accessor background
               :initarg :background
               :initform :default
               :type color)
   (font :accessor font
         :initarg :font
         :initform :default
         :type font)))

(defclass pretty-stream (inravina:pretty-stream)
  ())

(defclass ansi-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((style :accessor style
          :initform (make-instance 'ansi-style)
          :type ansi-style)
   (target :reader target
           :initarg :target)))

;;; Gray Stream protocol support

(defmethod trivial-gray-streams:stream-file-position ((stream ansi-stream))
  (file-position (target stream)))

(defmethod trivial-gray-streams:stream-write-char ((stream ansi-stream) char)
  (write-char char (target stream)))

(defmethod trivial-gray-streams:stream-write-string ((stream ansi-stream) string &optional start end)
  (write-string string (target stream) :start (or start 0) :end end))

(defmethod trivial-gray-streams:stream-finish-output ((stream ansi-stream))
  (finish-output (target stream)))

(defmethod trivial-gray-streams:stream-force-output ((stream ansi-stream))
  (force-output (target stream)))

(defmethod trivial-gray-streams:stream-clear-output ((stream ansi-stream))
  (clear-output (target stream)))

(defmethod trivial-gray-streams:stream-terpri ((stream ansi-stream))
  (terpri (target stream)))

(defmethod trivial-gray-streams:stream-fresh-line ((stream ansi-stream))
  (fresh-line (target stream)))

(defmethod trivial-gray-streams:stream-line-column ((stream ansi-stream))
  (trivial-stream-column:line-column (target stream)))

(defmethod trivial-gray-streams:stream-advance-to-column ((stream ansi-stream) column)
  (trivial-stream-column:advance-to-column column (target stream)))

;;; trivial-stream-column protocal support

(defmethod trivial-stream-column:stream-style ((stream ansi-stream))
  (style stream))

(defun write-ansi (value stream)
  (format stream "~C[~Am" #\Escape value))

(defmethod (setf trivial-stream-column:stream-style) (new-style (stream ansi-stream))
  (let ((foreground (foreground new-style))
        (background (background new-style))
        (font (font new-style))
        (target (target stream)))
    (write-ansi 0 target)
    (unless (eql :default foreground)
      (write-ansi (case foreground
                    (:black 30)
                    (:red 31)
                    (:green 32)
                    (:yellow 33)
                    (:blue 34)
                    (:magenta 35)
                    (:cyan 36)
                    (:white 37))
                  target))
    (unless (eql :default background)
      (write-ansi (case background
                    (:black 40)
                    (:red 41)
                    (:green 42)
                    (:yellow 43)
                    (:blue 44)
                    (:magenta 45)
                    (:cyan 46)
                    (:white 47))
                  target))
    (unless (eql :default font)
      (write-ansi (case font
                    (:bold 1)
                    (:faint 2)
                    (:italic 3)
                    (:underline 4)
                    (:slow-blink 5)
                    (:rapid-blink 6)
                    (:invert 7))
                  target))
    (setf (style stream) new-style)))

(defmethod trivial-stream-column:stream-copy-style ((stream ansi-stream) style &rest overrides &key &allow-other-keys)
  (make-instance 'ansi-style
                 :foreground (getf overrides :foreground (foreground style))
                 :background (getf overrides :background (background style))
                 :font (getf overrides :font (font style))))

(defmethod trivial-stream-column:stream-style ((stream ansi-stream))
  (style stream))

(defparameter *styles*
  (list :number (make-instance 'ansi-style :foreground :yellow)
        :dynamic-variable (make-instance 'ansi-style :foreground :cyan)
        :constant-variable (make-instance 'ansi-style :foreground :cyan :font :bold)))

(defmethod inravina:get-named-style (client (stream pretty-stream) name)
  (trivial-stream-column:stream-copy-style stream (getf *styles* name)))

