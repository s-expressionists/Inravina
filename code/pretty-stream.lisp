(in-package #:inravina)

(defclass logical-block ()
  ())

(defclass chunk ()
  ())

(defclass text-chunk ()
  ((value
     :accessor chunk-value
     :initform (make-array 32 :adjustable t :fill-pointer 0 :element-type 'character))))

(defclass newline-chunk ()
  ((kind
     :accessor chunk-kind
     :initarg :kind)))

(defclass tab-chunk ()
  ((kind
     :accessor chunk-kind
     :initarg :kind)
   (colnum
     :accessor chunk-colnum
     :initarg :colnum)
   (colinc
     :accessor chunk-colinc
     :initarg :colinc)))

(defclass start-chunk ()
  ())

(defclass end-chunk ()
  ())

(defclass pretty-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((stream
     :reader pretty-stream-stream
     :initarg :stream)
   (line
     :accessor pretty-stream-line
     :initform 0)
   (column
     :accessor pretty-stream-column
     :initform 0)
   (chunks
     :accessor pretty-stream-chunks
     :initform (make-instance 'queue))
   (logical-blocks
     :accessor pretty-stream-logical-blocks
     :initform (list (make-instance 'logical-block)))))

(defmethod initialize-instance :after ((instance pretty-stream) &rest initargs &key &allow-other-keys)
  (when (slot-boundp instance 'stream)
    (setf (pretty-stream-column instance)
          (or (ignore-errors
                (trivial-gray-streams:stream-line-column (pretty-stream-stream instance)))
              0))))

(defmethod pprint-newline (client kind (stream pretty-stream))
  (enqueue (make-instance 'newline-chunk :kind kind) (pretty-stream-chunks stream)))

(defmethod pprint-tab (client kind colnum colinc (stream pretty-stream))
  (enqueue (make-instance 'tab-chunk :kind kind :colnum colnum :colinc colinc)
           (pretty-stream-chunks stream)))

(defun enqueue-char (stream ch &aux (queue (pretty-stream queue)) (chunk (cdr (queue-tail-cons queue))))
  (unless (typep chunk 'text-chunk)
    (setf chunk (enqueue (make-instance 'text-chunk) (pretty-stream-chunks stream))))
  (vector-push-extend ch (chunk-text chunk)))

(defmethod trivial-gray-streams:stream-file-position ((stream pretty-stream))
  (file-position (pretty-stream-stream stream)))

(defmethod trivial-gray-streams:stream-write-char ((stream pretty-stream) char)
  (cond
    ((char= char #\Newline)
      (pprint-newline *client* :mandatory stream))
    ((char= char #\Tab)
      (pprint-tab *client* :line nil nil stream))
    ((graphic-char-p char)
      (enqueue-char (pretty-stream-column stream) char)))
  char)

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

