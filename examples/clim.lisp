(asdf:load-system "clim-listener")
(asdf:load-system :inravina/ext.extrinsic)

(in-package :clim-user)

(defun normalize-font-size (size)
  (cond ((null size)
         (let ((size* (clim:text-style-size clim:*default-text-style*)))
           (etypecase size*
             (number size*)
             (symbol (getf climi::+font-sizes+ size* nil)))))
        ((eq size :smaller)
         (getf climi::+font-sizes+ (clim:text-style-size clim:*default-text-style*) nil))
        ((eq size :larger)
         (getf climi::+font-sizes+ (clim:text-style-size clim:*default-text-style*) nil))
        ((realp size)
         (round (max size 2)))
        ((getf climi::+font-sizes+ size nil))
        (t
         (error "~s is not a valid text style size!" size))))

(defstruct style
  ink
  text-style)

(defmethod trivial-stream-column:stream-measure-string
    ((stream clim:standard-extended-output-stream) string &optional start end style)
  (/ (clim:stream-string-width stream string
                               :start start :end end
                               :text-style (and style (style-text-style style)))
     (normalize-font-size (clim:text-style-size (clim:medium-text-style stream)))))

(defmethod trivial-stream-column:stream-measure-char
    ((stream clim:standard-extended-output-stream) char &optional style)
  (/ (clim:stream-character-width stream char
                                  :text-style (and style (style-text-style style)))
     (normalize-font-size (clim:text-style-size (clim:medium-text-style stream)))))

(defmethod trivial-gray-streams:stream-advance-to-column
    ((stream clim:standard-extended-output-stream) column)
  (setf (clim:stream-cursor-position stream)
        (values (* (normalize-font-size (clim:text-style-size (clim:medium-text-style stream)))
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

#+(or)(defmethod incless:print-object-using-client :around (client (sym symbol) stream)
  (if (eq (find-symbol (symbol-name sym) :cl) sym)
      (let ((old-style (trivial-stream-column:stream-style stream)))
        (setf (trivial-stream-column:stream-style stream)
              (trivial-stream-column:stream-copy-style stream nil :ink clim:+cadet-blue+))
        (call-next-method)
        (setf (trivial-stream-column:stream-style stream) old-style))
      (call-next-method)))

(defmethod incless:print-object-using-client :around (client (object number) stream)
  (let ((old-style (trivial-stream-column:stream-style stream)))
    (setf (trivial-stream-column:stream-style stream)
          (trivial-stream-column:stream-copy-style stream nil :ink clim:+cadet-blue+))
    (call-next-method)
    (setf (trivial-stream-column:stream-style stream) old-style)))

(defmethod incless:print-object-using-client :around (client (object string) stream)
  (let ((old-style (trivial-stream-column:stream-style stream)))
    (setf (trivial-stream-column:stream-style stream)
          (trivial-stream-column:stream-copy-style stream nil :ink clim:+dark-green+))
    (call-next-method)
    (setf (trivial-stream-column:stream-style stream) old-style)))

(clim-listener:run-listener)

