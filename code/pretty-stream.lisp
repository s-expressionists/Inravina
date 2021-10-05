(in-package #:inravina)

(defclass chunk ()
  ((parent
     :accessor parent
     :initarg :parent)
   (line
     :accessor %line
     :initarg :line
     :initform nil
     :type (or null integer))
   (column
     :accessor %column
     :initarg :column
     :initform nil
     :type (or null integer))))

(defclass section-start (chunk)
  ((depth
     :accessor depth
     :initarg :depth
     :initform 0
     :type integer)
   (section-end
     :accessor section-end
     :initarg :section-end
     :initform nil
     :type (or null newline block-end))))

(defclass text (chunk)
  ((value
     :accessor value
     :initarg :value
     :initform "")))

(defclass indent (chunk)
  ((kind
     :accessor kind
     :initarg :kind
     :type (member :block :current))
   (width
     :accessor width
     :initarg :width)))

(defclass newline (section-start)
  ((kind
     :accessor kind
     :initarg :kind
     :type (member :linear :fill :miser :literal :mandatory))))

(defclass tab (chunk)
  ((kind
     :accessor kind
     :initarg :kind
     :type (member :line :line-relative :section :section-relative))
   (colnum
     :accessor colnum
     :initarg :colnum)
   (colinc
     :accessor colinc
     :initarg :colinc)))

(defclass block-start (section-start)
  ((prefix
     :accessor prefix
     :initarg :prefix)
   (per-line-prefix
     :accessor per-line-prefix
     :initarg :per-line-prefix)
   (suffix
     :accessor suffix
     :initarg :suffix)
   (start-column
     :accessor start-column
     :initarg :start-column
     :initform 0
     :type real)
   (section-column
     :accessor section-column
     :initarg :section-column
     :initform 0
     :type real)
   (block-end
     :accessor block-end
     :initarg :block-end
     :initform nil
     :type (or null block-end))))

(defclass block-end (chunk)
  ())

(defclass pretty-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((stream
     :reader pretty-stream-stream
     :initarg :stream)
   (client
     :reader pretty-stream-client
     :initarg :client)
   (line
     :accessor pretty-stream-line
     :initform 0)
   (column
     :accessor pretty-stream-column
     :initform 0)
   (chunks
     :accessor pretty-stream-chunks
     :initform (make-array 32 :adjustable t :fill-pointer 0 :element-type 'chunk))
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

(defun write-string+ (client instance string)
  (write-string string (pretty-stream-stream instance))
  (incf (pretty-stream-column instance) (text-width client instance string)))

(defun process-queue (instance &aux (client (pretty-stream-client instance)))
  (unless (pretty-stream-logical-blocks instance)
    (loop for chunk across (pretty-stream-chunks instance)
          do (write-chunk client instance chunk))))

(defgeneric write-chunk (client stream chunk))

(defmethod write-chunk (client stream chunk)
  (declare (ignore client stream chunk)))

(defmethod write-chunk (client stream (chunk text))
  (write-string+ client stream (value chunk))
  (incf (pretty-stream-column stream)
        (text-width client stream (value chunk))))

(defmethod write-chunk (client stream (chunk tab))
  (with-accessors ((column pretty-stream-column)) stream
    (cond
      ((< column (colnum chunk))
        (setf (column client stream) (colnum chunk)))
      ((not (zerop (colinc chunk)))
        (setf (column client stream)
              (+ (colnum chunk)
                 (* (colinc chunk)
                    (floor (+ column (- (colnum chunk)) (colinc chunk))
                           (colinc chunk)))))))))

(defmethod write-chunk (client stream (chunk newline))
  (when (or (eq :literal (kind chunk))
            (eq :mandatory (kind chunk)))
    (write-char #\newline stream)
    (setf (pretty-stream-column stream) 0)
    (incf (pretty-stream-line stream))))

(defmethod write-chunk (client stream (chunk block-start))
  (write-string+ client stream
                 (or (per-line-prefix chunk)
                     (prefix chunk))))

(defmethod write-chunk (client stream (chunk block-end))
  (write-string+ client stream
                            (suffix (parent chunk))))

(defmethod pprint-newline (client kind (stream pretty-stream))
  (with-accessors ((chunks pretty-stream-chunks))
                  stream
    (let* ((depth (length (pretty-stream-logical-blocks stream)))
           (newline (make-instance 'newline :kind kind
                                   :parent (car (pretty-stream-logical-blocks stream)))))
      (loop for chunk across chunks
            when (and (typep chunk 'section-start)
                      (null (section-end chunk))
                      (<= depth (depth chunk)))
            do (setf (section-end chunk) newline))
      (vector-push-extend newline chunks)
      (process-queue stream))))

(defmethod pprint-tab (client kind colnum colinc (stream pretty-stream))
  (vector-push-extend (make-instance 'tab :kind kind :colnum colnum :colinc colinc
                                     :parent (car (pretty-stream-logical-blocks stream)))
                      (pretty-stream-chunks stream)))

(defmethod pprint-indent (client relative-to n (stream pretty-stream))
  (vector-push-extend (make-instance 'indent :kind relative-to :width n
                                     :parent (car (pretty-stream-logical-blocks stream)))
                      (pretty-stream-chunks stream)))

(defmethod trivial-gray-streams:stream-file-position ((stream pretty-stream))
  (file-position (pretty-stream-stream stream)))

(defmethod trivial-gray-streams:stream-write-char ((stream pretty-stream) char)
  (if (char= char #\newline)
    (terpri stream)
    (with-accessors ((chunks pretty-stream-chunks))
                    stream
      (let ((chunk (unless (zerop (length chunks)) (aref chunks (1- (length chunks))))))
        (if (typep chunk 'text)
          (setf (value chunk) (concatenate 'string (value chunk) (string char)))
          (vector-push-extend (make-instance 'text
                                             :value (string char)
                                             :parent (car (pretty-stream-logical-blocks stream)))
                              chunks)))))
  char)

(defun enqueue-string (stream string &optional (start 0) end)
  (with-accessors ((chunks pretty-stream-chunks))
                  stream
    (let ((chunk (unless (zerop (length chunks)) (aref chunks (1- (length chunks))))))
      (if (typep chunk 'text)
        (setf (value chunk)
              (concatenate 'string (value chunk) (subseq string start end)))
        (vector-push-extend (make-instance 'text
                                           :value (subseq string start end)
                                           :parent (car (pretty-stream-logical-blocks stream)))
                            chunks)))))

(defmethod trivial-gray-streams:stream-write-string ((stream pretty-stream) string &optional start end)
  (prog (pos)
   next
    (setf pos (position #\newline string :start (or start 0) :end end))
    (when pos
      (enqueue-string stream string start pos)
      (terpri stream)
      (setf start (1+ pos))
      (go next))
    (enqueue-string stream string start end)))

(defmethod trivial-gray-streams:stream-finish-output ((stream pretty-stream))
  (process-queue stream))

(defmethod trivial-gray-streams:stream-terpri ((stream pretty-stream))
  (vector-push-extend (make-instance 'newline :kind :literal
                                     :parent (car (pretty-stream-logical-blocks stream)))
                      (pretty-stream-chunks stream))
  (process-queue stream))

(defmethod trivial-gray-streams:stream-line-column ((stream pretty-stream))
  (pretty-stream-column stream))

(defmethod make-pretty-stream ((client client) (stream pretty-stream))
  stream)

(defmethod make-pretty-stream ((client client) (stream (eql nil)))
  (make-instance 'pretty-stream :stream *standard-output* :client client))

(defmethod make-pretty-stream ((client client) (stream (eql t)))
  (make-instance 'pretty-stream :stream *terminal-io* :client client))

(defmethod make-pretty-stream ((client client) stream)
  (make-instance 'pretty-stream :stream stream :client client))

(defmethod pprint-start-logical-block (client (stream pretty-stream) prefix per-line-prefix suffix)
  (let ((block-start (make-instance 'block-start
                                    :prefix prefix
                                    :per-line-prefix per-line-prefix
                                    :suffix suffix
                                    :depth (length (pretty-stream-logical-blocks stream))
                                    :parent (car (pretty-stream-logical-blocks stream)))))
    (push block-start (pretty-stream-logical-blocks stream))
    (vector-push-extend block-start (pretty-stream-chunks stream))))

(defmethod pprint-end-logical-block (client (stream pretty-stream))
  (let ((block-end (make-instance 'block-end :parent (car (pretty-stream-logical-blocks stream)))))
    ;(setf (block-end (car (pretty-stream-logical-blocks stream))) block-end)
    (pop (pretty-stream-logical-blocks stream))
    (vector-push-extend block-end (pretty-stream-chunks stream))
    (process-queue stream)))

(defmethod (setf column) (new-value client (stream pretty-stream))
  (dotimes (i (- new-value (column client stream)))
    (declare (ignore i))
    (write-char #\Space (pretty-stream-stream stream)))
  (setf (pretty-stream-column stream) new-value))

(defmethod (setf line) (new-value client (stream pretty-stream))
  (dotimes (i (- new-value (line client stream)))
    (declare (ignore i))
    (terpri (pretty-stream-stream stream)))
  (setf (pretty-stream-line stream) new-value
        (pretty-stream-column stream) 0))

(defmethod column (client (stream pretty-stream))
  (pretty-stream-column stream))

(defmethod line (client (stream pretty-stream))
  (pretty-stream-line stream))  

