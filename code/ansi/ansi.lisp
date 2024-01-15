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

(defclass ansi-stream (ngray:fundamental-character-output-stream)
  ((style :accessor style
          :initform (make-instance 'ansi-style)
          :type ansi-style)
   (target :reader target
           :initarg :target)))

;;; Gray Stream protocol support

(defmethod ngray:stream-file-position ((stream ansi-stream))
  (file-position (target stream)))

(defmethod ngray:stream-write-char ((stream ansi-stream) char)
  (write-char char (target stream)))

(defmethod ngray:stream-write-string ((stream ansi-stream) string &optional start end)
  (write-string string (target stream) :start (or start 0) :end end))

(defmethod ngray:stream-finish-output ((stream ansi-stream))
  (finish-output (target stream)))

(defmethod ngray:stream-force-output ((stream ansi-stream))
  (force-output (target stream)))

(defmethod ngray:stream-clear-output ((stream ansi-stream))
  (clear-output (target stream)))

(defmethod ngray:stream-terpri ((stream ansi-stream))
  (terpri (target stream)))

(defmethod ngray:stream-fresh-line ((stream ansi-stream))
  (fresh-line (target stream)))

(defmethod ngray:stream-line-column ((stream ansi-stream))
  (ngray:stream-line-column (target stream)))

(defmethod ngray:stream-advance-to-column ((stream ansi-stream) column)
  (ngray:stream-advance-to-column (target stream) column))

;;; style protocal support

(defmethod inravina:stream-style ((stream ansi-stream))
  (style stream))

(defun write-ansi (value stream)
  (format stream "~C[~Am" #\Escape value))

(defparameter *styles*
  (list :number (make-instance 'ansi-style :foreground :yellow)
        :dynamic-variable (make-instance 'ansi-style :foreground :cyan)
        :constant-variable (make-instance 'ansi-style :foreground :cyan :font :bold)))

(defmethod inravina:make-style
    (client (stream ansi-stream) &rest initargs
     &key (name nil namep) foreground background font)
  (let ((named-style (when namep (getf *styles* name))))
    (make-instance 'ansi-style
                   :foreground (or foreground
                                   (and named-style
                                        (foreground named-style))
                                   :default)
                   :background (or background
                                   (and named-style
                                        (background named-style))
                                   :default)
                   :font (or font
                             (and named-style
                                  (font named-style))
                             :default))))

(defmethod (setf inravina:stream-style) (new-style (stream ansi-stream))
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

(defmethod inravina:stream-style ((stream ansi-stream))
  (style stream))
