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

(defmethod trivial-stream-column:stream-measure-string
    ((stream clim:standard-extended-output-stream) string &optional start end)
  (/ (clim:stream-string-width stream string :start start :end end)
     (normalize-font-size (clim:text-style-size (clim:medium-text-style stream)))))

(defmethod trivial-stream-column:stream-measure-char
    ((stream clim:standard-extended-output-stream) char)
  (/ (clim:stream-character-width stream char)
     (normalize-font-size (clim:text-style-size (clim:medium-text-style stream)))))

(defmethod trivial-gray-streams:stream-advance-to-column
    ((stream clim:standard-extended-output-stream) column)
  (setf (clim:stream-cursor-position stream)
        (values (* (normalize-font-size (clim:text-style-size (clim:medium-text-style stream)))
                   column)
                (nth-value 1 (clim:stream-cursor-position stream))))
  column)

(clim-listener:run-listener)

