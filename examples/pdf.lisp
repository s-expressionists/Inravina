(require :inravina/ext.extrinsic)

(require :cl-pdf)

(defclass pdf-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((line :accessor line
         :initform 0)
   (column :accessor column
           :initform 0)
   (indent :accessor indent
           :initform 0)))

(defun em-size ()
  (pdf:get-char-width #\M pdf::*font* pdf::*font-size*))

(defun measure-string (text)
  (/ (pdf::text-width text pdf::*font* pdf::*font-size*)
     (em-size)))

(defun measure-char (text)
  (/ (pdf:get-char-width text pdf::*font* pdf::*font-size*)
     (em-size)))

(defmethod trivial-stream-column:stream-measure-string ((stream pdf-stream) string &optional start end style)
  (declare (ignore stream style))
  (measure-string (subseq string (or start 0) end)))

(defmethod trivial-stream-column:stream-measure-char ((stream pdf-stream) char &optional style)
  (declare (ignore stream style))
  (measure-char char))

(defmethod trivial-gray-streams:stream-terpri ((stream pdf-stream))
  #+(or) (pdf:move-to-next-line)
  (pdf:move-text (* (- (indent stream)) (em-size))
                 (* pdf::*font-size* -1.2))
  (incf (line stream))
  (setf (column stream) 0
        (indent stream) 0))

(defmethod trivial-gray-streams:stream-advance-to-column ((stream pdf-stream) column)
  (unless (zerop column)
    (pdf:move-text (* column (em-size))
                   0)
    (setf (column stream) column
          (indent stream) column)))

(defmethod trivial-gray-streams:stream-write-char ((stream pdf-stream) char)
  (pdf:draw-text (string char))
  (incf (column stream) (measure-char char))
  char)

(defmethod trivial-gray-streams:stream-write-string ((stream pdf-stream) string &optional start end)
  (pdf:draw-text (subseq string start end))
  (incf (column stream) (measure-string (subseq string start end)))
  string)

(defmethod trivial-gray-streams:stream-line-column ((stream pdf-stream))
  (column stream))

(pdf:with-document ()
  (pdf:with-page ()
    (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	    (let ((helvetica (pdf:get-font "Helvetica")))
	      (pdf:in-text-mode
	        (pdf:move-text 100 800)
	        (pdf:set-font helvetica 12.0)
	        (pdf:set-text-leading (* 1.2 12.0))
	        (incless:pprint '(if (quux 'a)
	                             (loop for i in '(1 2 3) do (print i) (wibble i) collect i)
	                             (write-line "gronk"))
	                        (make-instance 'pdf-stream))))))
    (pdf:write-document #P"t.pdf"))
