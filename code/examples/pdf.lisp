(in-package #:inravina-examples)

(defclass pdf-stream (ngray:fundamental-character-output-stream)
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

(defmethod inravina:stream-measure-string ((stream pdf-stream) string &optional style previous-char)
  (declare (ignore stream style previous-char))
  (measure-string string))

(defmethod inravina:stream-measure-char ((stream pdf-stream) char &optional style)
  (declare (ignore stream style))
  (measure-char char))

(defmethod ngray:stream-terpri ((stream pdf-stream))
  (pdf:move-text (* (- (indent stream)) (em-size))
                 (* pdf::*font-size* -1.2))
  (incf (line stream))
  (setf (column stream) 0
        (indent stream) 0))

(defmethod ngray:stream-advance-to-column ((stream pdf-stream) column)
  (unless (zerop column)
    (pdf:move-text (* column (em-size))
                   0)
    (setf (column stream) column
          (indent stream) column)))

(defmethod ngray:stream-write-char ((stream pdf-stream) char)
  (pdf:draw-text (string char))
  (incf (column stream) (measure-char char))
  char)

(defmethod ngray:stream-write-string ((stream pdf-stream) string &optional start end)
  (pdf:draw-text (subseq string (or start 0) end))
  (incf (column stream) (measure-string (subseq string start end)))
  string)

(defmethod ngray:stream-line-column ((stream pdf-stream))
  (column stream))

(defun pprint-pdf (form path &key (font "Helvetica"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
        (let ((helvetica (pdf:get-font font))
              (*print-pretty* t))
          (pdf:in-text-mode
           (pdf:move-text 100 800)
           (pdf:set-font helvetica 12.0)
           (pdf:set-text-leading (* 1.2 12.0))
           (incless-extrinsic:pprint form (make-instance 'pdf-stream))))))
    (pdf:write-document path)))
