(in-package #:inravina)

(defclass logical-block ()
  ((prefix
     :accessor logical-block-prefix
     :initarg :prefix)
   (per-line-prefix
     :accessor logical-block-per-line-prefix
     :initarg :per-line-prefix)
   (suffix
     :accessor logical-block-suffix
     :initarg :suffix)))

(defclass chunk ()
  ((logical-block
     :accessor chunk-logical-block
     :initarg :logical-block)))

(defclass kind-chunk (chunk)
  ((kind
     :accessor chunk-kind
     :initarg :kind)))

(defclass text-chunk (chunk)
  ((value
     :accessor chunk-value
     :initarg :value
     :initform "")))

(defclass newline-chunk (kind-chunk)
  ())

(defclass tab-chunk (kind-chunk)
  ((colnum
     :accessor chunk-colnum
     :initarg :colnum)
   (colinc
     :accessor chunk-colinc
     :initarg :colinc)))

(defclass logical-block-chunk (chunk)
 ())

(defclass start-chunk (logical-block-chunk)
  ())

(defclass end-chunk (logical-block-chunk)
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
     :initform nil)))

(defmethod initialize-instance :after ((instance pretty-stream) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (slot-boundp instance 'stream)
    (setf (pretty-stream-column instance)
          (or (ignore-errors
                (trivial-gray-streams:stream-line-column (pretty-stream-stream instance)))
              0))))

(defun process-queue (instance)
  (with-accessors ((chunks pretty-stream-chunks)
                   (stream pretty-stream-stream)
                   (logical-blocks pretty-stream-logical-blocks))
                  instance
    (prog (chunk)
     next
      (unless (or logical-blocks (queue-empty-p chunks))
        (setf chunk (pop-head chunks))
        (etypecase chunk
          (text-chunk (write-string (chunk-value chunk) stream))
          (tab-chunk (write-char #\tab stream))
          (newline-chunk (when (eq :mandatory (chunk-kind chunk)) (write-char #\newline stream)))
          (start-chunk (write-string (or (logical-block-per-line-prefix (chunk-logical-block chunk))
                                         (logical-block-prefix (chunk-logical-block chunk)))
                                     stream))
          (end-chunk (write-string (logical-block-suffix (chunk-logical-block chunk))
                                   stream)))
        (go next)))))

(defmethod pprint-newline (client kind (stream pretty-stream))
  (push-tail (pretty-stream-chunks stream)
             (make-instance 'newline-chunk :kind kind
                            :logical-block (car (pretty-stream-logical-blocks stream))))
  (process-queue stream))

(defmethod pprint-tab (client kind colnum colinc (stream pretty-stream))
  (push-tail (pretty-stream-chunks stream)
             (make-instance 'tab-chunk :kind kind :colnum colnum :colinc colinc
                            :logical-block (car (pretty-stream-logical-blocks stream)))))

(defmethod trivial-gray-streams:stream-file-position ((stream pretty-stream))
  (file-position (pretty-stream-stream stream)))

(defmethod trivial-gray-streams:stream-write-char ((stream pretty-stream) char)
  (with-accessors ((chunks pretty-stream-chunks))
                  stream
    (let ((chunk (tail chunks)))
      (if (typep chunk 'text-chunk)
        (setf (chunk-value chunk) (concatenate 'string (chunk-value chunk) (string char)))
        (push-tail chunks
                   (make-instance 'text-chunk
                                  :value (string char)
                                  :logical-block (car (pretty-stream-logical-blocks stream)))))))
  char)

(defmethod trivial-gray-streams:stream-write-string ((stream pretty-stream) string &optional start end)
  (with-accessors ((chunks pretty-stream-chunks))
                  stream
    (let ((chunk (tail chunks))
          (string (if (or start end)
                    (subseq string (or start 0) end)
                    string)))
      (if (typep chunk 'text-chunk)
        (setf (chunk-value chunk) (concatenate 'string (chunk-value chunk) string))
        (push-tail chunks
                   (make-instance 'text-chunk
                                  :value string
                                  :logical-block (car (pretty-stream-logical-blocks stream)))))))
  string)

(defmethod trivial-gray-streams:stream-finish-output ((stream pretty-stream))
  (process-queue stream))

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

(defmethod pprint-start-logical-block (client (stream pretty-stream) prefix per-line-prefix suffix)
  (let ((logical-block (make-instance 'logical-block
                                      :prefix prefix
                                      :per-line-prefix per-line-prefix
                                      :suffix suffix)))
    (push logical-block (pretty-stream-logical-blocks stream))
    (push-tail (pretty-stream-chunks stream)
               (make-instance 'start-chunk :logical-block logical-block))))

(defmethod pprint-end-logical-block (client (stream pretty-stream))
  (push-tail (pretty-stream-chunks stream)
             (make-instance 'end-chunk :logical-block (pop (pretty-stream-logical-blocks stream))))
  (process-queue stream))


