(in-package #:inravina)

(defmethod text-width (client stream (text string) &optional start end)
  (declare (ignore client stream))
  (- (or end (length text))
     (or start 0)))

(defmethod text-width (client stream (text character) &optional start end)
  (declare (ignore client stream start end))
  1)

(defmethod text-width (client stream (text (eql nil)) &optional start end)
  (declare (ignore client stream start end))
  0)

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
      (pprint-newline client stream :literal-mandatory)
      (setf start (1+ pos))
      (go next))
    (pprint-text client stream text start end)))

(defvar *circularity-hash-table* nil)

(defvar *circularity-counter* nil)

(defun uniquely-identified-by-print-p (x)
  (or (numberp x)
      (characterp x)
      (symbolp x)))

(defmethod write-object (client stream object)
  (flet ((do-print (stream)
           (when (and *circularity-hash-table*
                      (not (uniquely-identified-by-print-p object)))
             (multiple-value-bind (current presentp)
                 (gethash object *circularity-hash-table*)
               (cond ((and *circularity-hash-table*
                           *circularity-counter*
                           (eq t current))
                      (setf (gethash object *circularity-hash-table*)
                            (incf *circularity-counter*))
                      (write-char #\# stream)
                      (write *circularity-counter* :stream stream)
                      (write-char #\= stream))
                     ((and *circularity-hash-table*
                           *circularity-counter*
                           current)
                      (write-char #\# stream)
                      (write *circularity-counter* :stream stream)
                      (write-char #\# stream)
                      (return-from do-print))
                   (*circularity-hash-table*
                    (setf (gethash object *circularity-hash-table*) presentp)))))
           (if *print-pretty*
               (funcall (pprint-dispatch client *print-pprint-dispatch* object)
                        stream object)
               (print-object object stream))))
    (cond ((and *print-circle*
                (not *circularity-hash-table*))
           (let ((*circularity-hash-table* (make-hash-table :test #'eq))
                 (*circularity-counter* nil))
             (do-print (make-broadcast-stream))
             (setf *circularity-counter* 0)
             (do-print stream)))
          (t
           (do-print stream))))
  nil)
