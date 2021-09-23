(in-package #:inravina)

(defclass pretty-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((stream
     :reader pretty-stream-stream
     :initarg :stream)
   (column
     :accessor pretty-stream-column
     :initform 0)))

(defmethod trivial-gray-streams:stream-file-position ((stream pretty-stream))
  (file-position (pretty-stream-stream stream)))

(defmethod trivial-gray-streams:stream-write-char ((stream pretty-stream) char)
  (cond
    ((char= char #\Newline)
      (setf (pretty-stream-column stream) 0))
    ((char= char #\Tab)
      (incf (pretty-stream-column stream) 8))
    ((graphic-char-p char)
      (incf (pretty-stream-column stream))))
  (write-char char (pretty-stream-stream stream)))

(defmethod trivial-gray-streams:stream-finish-output ((stream pretty-stream))
  (finish-output (pretty-stream-stream stream)))

(defmethod trivial-gray-streams:stream-line-column ((stream pretty-stream))
  (pretty-stream-column stream))

(defmethod make-pretty-stream ((client client) (stream pretty-stream))
  stream)

(defmethod make-pretty-stream ((client client) (stream (eql nil)))
  (make-instance 'pretty-stream :stream *standard-output*))

(defmethod make-pretty-stream ((client client) (stream (eql t)))
  (make-instance 'pretty-stream :stream *terminal-io*))

(defmethod make-pretty-stream ((client client) stream)
  (make-instance 'pretty-stream :stream stream))
