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
   (single-line
     :accessor single-line
     :initarg :single-line
     :initform t
     :type boolean)
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
  ((suffix
     :accessor suffix
     :initarg :suffix)))

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

(defun process-queue (stream &aux (client (pretty-stream-client stream))
                      (chunks (pretty-stream-chunks stream)))
  (unless (pretty-stream-logical-blocks stream)
    (loop for chunk across chunks
          for i from 0
          for block-end = (find-if (lambda (x) (typep x 'block-end))
                                   chunks
                                   :from-end t
                                   :start i)
          when (and block-end
                    (typep chunk 'newline)
                    (not (section-end chunk)))
          do (setf (section-end chunk) block-end))
    ; (loop for chunk across chunks
    ;       with indent = 0
    ;       do (typecase chunk
    ;            (block-start (write-char #\< (pretty-stream-stream stream)))
    ;            (block-end (write-char #\> (pretty-stream-stream stream)))
    ;            (newline (format (pretty-stream-stream stream) "~36R" (incf indent)))
    ;            (text (write-string (make-string (length (value chunk)) :initial-element #\-) (pretty-stream-stream stream)))))
    ; (terpri (pretty-stream-stream stream))
    ; (loop for newline across chunks
    ;       with indent = 0
    ;       when (typep newline 'newline)
    ;       do (progn
    ;            (incf indent)
    ;            (loop for chunk across chunks
    ;                  with section = nil
    ;                  do (typecase chunk
    ;                       (section-start
    ;                         (cond
    ;                           ((eq (section-end chunk) newline)
    ;                             (write-char #\[ (pretty-stream-stream stream))
    ;                             (setf section t))
    ;                           ((eq chunk newline)
    ;                             (format (pretty-stream-stream stream) "~36R" indent)
    ;                             (setf section t))
    ;                           ((eq (section-end newline) chunk)
    ;                             (write-char #\] (pretty-stream-stream stream))
    ;                             (setf section nil))
    ;                           (t
    ;                             (write-char (if section #\- #\Space) (pretty-stream-stream stream)))))
    ;                       (block-end
    ;                         (cond
    ;                           ((eq (section-end newline) chunk)
    ;                             (write-char #\] (pretty-stream-stream stream))
    ;                             (setf section nil))
    ;                           (t
    ;                             (write-char (if section #\- #\Space) (pretty-stream-stream stream)))))
    ;                       (text (write-string (make-string (length (value chunk)) :initial-element (if section #\- #\Space)) (pretty-stream-stream stream)))))
    ;            (terpri (pretty-stream-stream stream))))
    (prog ((i 0) sections success chunk)
     repeat
      (when (< i (length chunks))
        (setf chunk (aref chunks i)
              (%column chunk) (if (zerop i)
                                (pretty-stream-column stream)
                                (%column (aref chunks (1- i))))
              (%line chunk) (if (zerop i)
                              (pretty-stream-line stream)
                              (%line (aref chunks (1- i)))))
        (when (and sections
                   (typep chunk 'section-start)
                   (eq (section-end (first sections)) chunk))
          (pop sections))
        (setf success (layout client stream chunk
                              (when sections
                                (single-line (first sections)))))
        (when (and (not (eq (first sections) chunk))
                   (typep chunk 'section-start))
          (push chunk sections)
          (setf (single-line chunk) t))
        (cond
          ((and success
                sections
                (eq (section-end (first sections)) chunk))
            (pop sections)
            (incf i))
          (success
            (incf i))
          ((null sections)
            (error "Layout failure outside of a section."))
          ((single-line (first sections))
            (loop while (and (cdr sections)
                             (typep (second sections) 'section-start)
                             (single-line (second sections)))
                  do (pop sections))
            (setf i (position (first sections) chunks)
                  (single-line (first sections)) nil))
          (t
            (error "layout failure while in non-newline section in multiline mode.")))
        (go repeat)))
    (loop for chunk across chunks
          do (write-chunk client stream chunk))
    (setf (fill-pointer (pretty-stream-chunks stream)) 0)))

(defgeneric layout (client stream chunk single-line))

(defun layout-text (client stream chunk single-line text)
  (let ((width (text-width client stream text)))
    (unless (and single-line
                 (<= (right-margin client stream) (+ width (%column chunk))))
      (incf (%column chunk) width)
      t)))

(defmethod layout (client stream (chunk text) single-line)
  (layout-text client stream chunk single-line (value chunk)))

(defmethod layout (client stream (chunk tab) single-line)
  (with-accessors ((column %column)
                   (colnum colnum)
                   (colinc colinc))
                  chunk
    (let ((new-column (cond
                        ((< column colnum)
                          colnum)
                        ((not (zerop (colinc chunk)))
                          (+ colnum
                             (* colinc
                                (floor (+ column (- colnum) colinc)
                                       colinc)))))))
      (unless (and single-line
                   (<= (right-margin client stream) new-column))
        (setf column new-column)
        t))))

(defmethod layout (client stream (chunk newline) single-line)
  (cond
    ((and single-line
          (member (kind chunk) '(:literal :mandatory)))
      nil)
    ((or single-line
         (and (eq (kind chunk) :fill)
              (single-line chunk)))
      t)
    (t
      (setf (%column chunk) 0)
      (incf (%line chunk))
      t)))

(defmethod layout (client stream (chunk block-start) single-line)
  (let ((column (%column chunk)))
    (setf (start-column chunk) column
          (section-column chunk) column)
    (layout-text client stream chunk single-line
                       (or (per-line-prefix chunk)
                           (prefix chunk)))))

(defmethod layout (client stream (chunk block-end) single-line)
  (layout-text client stream chunk single-line (suffix chunk)))

(defgeneric write-chunk (client stream chunk))

(defmethod write-chunk (client stream chunk)
  (declare (ignore client stream chunk)))

(defmethod write-chunk :after (client (stream pretty-stream) chunk)
  (declare (ignore client))
  (setf (pretty-stream-line stream) (%line chunk)
        (pretty-stream-column stream) (%column chunk)))

(defmethod write-chunk (client (stream pretty-stream) (chunk text))
  (write-string (value chunk) (pretty-stream-stream stream)))

(defmethod write-chunk (client (stream pretty-stream) (chunk tab))
  (dotimes (i (- (%column chunk) (pretty-stream-column stream)))
    (declare (ignore i))
    (write-char #\Space (pretty-stream-stream stream))))

(defmethod write-chunk (client (stream pretty-stream) (chunk newline))
  (unless (eql (pretty-stream-line stream) (%line chunk))
  (write-char #\newline (pretty-stream-stream stream))))

(defmethod write-chunk (client (stream pretty-stream) (chunk block-start))
  (write-string (or (per-line-prefix chunk)
                    (prefix chunk))
                (pretty-stream-stream stream)))

(defmethod write-chunk (client (stream pretty-stream) (chunk block-end))
  (write-string (suffix chunk) (pretty-stream-stream stream)))

(defmethod pprint-newline (client kind (stream pretty-stream))
  (with-accessors ((chunks pretty-stream-chunks))
                  stream
    (let* ((depth (length (pretty-stream-logical-blocks stream)))
           (newline (make-instance 'newline :kind kind :depth depth
                                   :parent (car (pretty-stream-logical-blocks stream)))))
      (loop for chunk across chunks
            when (and (typep chunk 'section-start)
                      (null (section-end chunk))
                      (<= (depth chunk) depth))
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

(defmethod pprint-start-logical-block (client (stream pretty-stream) prefix per-line-prefix)
  (let ((block-start (make-instance 'block-start
                                    :prefix prefix
                                    :per-line-prefix per-line-prefix
                                    :depth (length (pretty-stream-logical-blocks stream))
                                    :parent (car (pretty-stream-logical-blocks stream)))))
    (push block-start (pretty-stream-logical-blocks stream))
    (vector-push-extend block-start (pretty-stream-chunks stream))))

(defmethod pprint-end-logical-block (client (stream pretty-stream) suffix)
  (let ((block-end (make-instance 'block-end :suffix suffix :parent (car (pretty-stream-logical-blocks stream)))))
    (setf (block-end (car (pretty-stream-logical-blocks stream))) block-end)
    (pop (pretty-stream-logical-blocks stream))
    (vector-push-extend block-end (pretty-stream-chunks stream))
    (process-queue stream)))

(defmethod column (client (stream pretty-stream))
  (pretty-stream-column stream))

(defmethod line (client (stream pretty-stream))
  (pretty-stream-line stream))  

