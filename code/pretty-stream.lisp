(in-package #:inravina)

(defclass chunk ()
  ((parent
     :accessor parent
     :initarg :parent)
   (index
     :accessor index
     :initarg :index
     :initform nil
     :type (or null integer))
   (line
     :accessor %line
     :initarg :line
     :initform nil
     :type (or null integer))
   (hard-column
     :accessor hard-column
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
   (indent
     :accessor indent
     :initarg :indent
     :initform nil
     :type (or null integer))
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
   (arranged-text
     :accessor arranged-text
     :initform (make-array 32 :adjustable t :fill-pointer 0 :element-type 'list))
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
          do (setf (section-end chunk) block-end)
          when (typep chunk 'text)
          do (setf (value chunk) (normalize-text client stream (value chunk))))
    (prog ((i 0) sections success chunk)
     repeat
      (when (< i (length chunks))
        (setf chunk (aref chunks i)
              (hard-column chunk) (if (zerop i)
                                (pretty-stream-column stream)
                                (hard-column (aref chunks (1- i))))
              (%column chunk) (if (zerop i)
                                (pretty-stream-column stream)
                                (%column (aref chunks (1- i))))
              (%line chunk) (if (zerop i)
                              (pretty-stream-line stream)
                              (%line (aref chunks (1- i))))
              (index chunk) (length (arranged-text stream)))
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
                  (single-line (first sections)) nil
                  (fill-pointer (arranged-text stream)) (index (first sections))))
          (t
            (error "layout failure while in non-newline section in multiline mode.")))
        (go repeat)))
    (map nil (lambda (args) (apply #'write-text client stream args)) (arranged-text stream))
    (setf (fill-pointer (arranged-text stream)) 0
          (fill-pointer (pretty-stream-chunks stream)) 0)))

(defmethod write-text (client (stream pretty-stream) line column text)
  (when line
    (loop while (< (pretty-stream-line stream) line)
          do (write-char #\Newline (pretty-stream-stream stream))
          do (incf (pretty-stream-line stream)))
    (setf (pretty-stream-column stream) 0))
  (when column
    (loop while (< (pretty-stream-column stream) column)
          do (write-char #\Space (pretty-stream-stream stream))
          do (incf (pretty-stream-column stream))))
  (when text
    (write-string text (pretty-stream-stream stream))
    (incf (pretty-stream-column stream) (text-width client stream text))))

(defgeneric layout (client stream chunk single-line))

(defun layout-arrange-text (client stream chunk single-line line column text)
  (let ((new-hard-column (hard-column chunk))
        (new-column (%column chunk))
        (new-line (%line chunk))
        (width (text-width client stream text))
        (hard-width (text-width client stream text 0 (break-position client stream text))))
    (when line
      (setf new-line line
            new-hard-column 0
            new-column 0))
    (when column
      (setf new-column column))
    (unless (zerop hard-width)
      (setf new-hard-column (+ new-column hard-width)))
    (incf new-column width)
    #+(or)(format t "Line: ~2d -> ~2d, Column: ~2d -> ~2d, Hard Column: ~2d -> ~2d, Text: ~s~%"
            (%line chunk) new-line
            (%column chunk) new-column
            (hard-column chunk) new-hard-column
            text)
    (unless (and single-line
                 (< (right-margin client stream) new-hard-column))
      (setf (hard-column chunk) new-hard-column
            (%column chunk) new-column
            (%line chunk) new-line)
      (vector-push-extend (list line column text) (arranged-text stream))
      t)))

(defmethod layout (client stream (chunk text) single-line)
  (layout-arrange-text client stream chunk single-line nil nil (value chunk)))

(defmethod layout (client stream (chunk tab) single-line)
  (with-accessors ((column %column)
                   (colnum colnum)
                   (colinc colinc))
                  chunk
    (layout-arrange-text client stream chunk single-line nil
                         (cond
                           ((< column colnum)
                             colnum)
                           ((not (zerop (colinc chunk)))
                             (+ colnum
                                (* colinc
                                   (floor (+ column (- colnum) colinc)
                                          colinc)))))
                         nil)))

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
      (layout-arrange-text client stream chunk single-line
                           (1+ (%line chunk))
                           (if (eq (kind chunk) :literal)
                             0
                             (indent (parent chunk)))
                           nil))))

(defmethod layout (client stream (chunk indent) single-line)
  (setf (indent (parent chunk))
        (+ (width chunk)
           (ecase (kind chunk)
             (:block
               (start-column (parent chunk)))
             (:current
               (%column chunk))))))

(defmethod layout (client stream (chunk block-start) single-line)
  (let ((column (%column chunk)))
    (setf (start-column chunk) column
          (section-column chunk) column
          (indent chunk) (+ column (text-width client stream (or (prefix chunk) (per-line-prefix chunk) ""))))
    (layout-arrange-text client stream chunk single-line nil nil
                       (or (per-line-prefix chunk)
                           (prefix chunk)))))

(defmethod layout (client stream (chunk block-end) single-line)
  (layout-arrange-text client stream chunk single-line nil nil (suffix chunk)))

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
                                    :prefix (normalize-text client stream prefix)
                                    :per-line-prefix (normalize-text client stream per-line-prefix)
                                    :depth (length (pretty-stream-logical-blocks stream))
                                    :parent (car (pretty-stream-logical-blocks stream)))))
    (push block-start (pretty-stream-logical-blocks stream))
    (vector-push-extend block-start (pretty-stream-chunks stream))))

(defmethod pprint-end-logical-block (client (stream pretty-stream) suffix)
  (let ((block-end (make-instance 'block-end
                                  :suffix (normalize-text client stream suffix)
                                  :parent (car (pretty-stream-logical-blocks stream)))))
    (setf (block-end (car (pretty-stream-logical-blocks stream))) block-end)
    (pop (pretty-stream-logical-blocks stream))
    (vector-push-extend block-end (pretty-stream-chunks stream))
    (process-queue stream)))

(defmethod column (client (stream pretty-stream))
  (pretty-stream-column stream))

(defmethod line (client (stream pretty-stream))
  (pretty-stream-line stream))  

