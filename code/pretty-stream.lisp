(in-package #:inravina)

(defclass instruction ()
  ((parent
     :accessor parent
     :initarg :parent)
   (index
     :accessor index
     :initarg :index
     :initform nil
     :type (or null integer))
   (line
     :accessor line
     :initarg :line
     :initform nil
     :type (or null integer))
   (break-column
     :accessor break-column
     :initform nil
     :type (or null integer))
   (column
     :accessor column
     :initarg :column
     :initform nil
     :type (or null integer))))

(defclass section-start (instruction)
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

(defclass text (instruction)
  ((value
     :accessor value
     :initarg :value
     :initform "")))

(defclass indent (instruction)
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
     :type (member :linear :fill :miser :literal-mandatory :mandatory))))

(defclass tab (instruction)
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
   (prefix-fragments
     :accessor prefix-fragments
     :initform nil)
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

(defclass block-end (instruction)
  ((suffix
     :accessor suffix
     :initarg :suffix)))

(defclass fragment ()
  ((line
     :accessor line
     :initarg :line
     :initform nil
     :type (or null integer))
   (column
     :accessor column
     :initarg :column
     :initform nil
     :type (or null integer))
   (text
     :accessor text
     :initarg :text
     :initform nil
     :type (or null string))))

(defclass pretty-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((target
     :reader target
     :initarg :target)
   (client
     :reader client
     :initarg :client)
   (line
     :accessor line
     :initform 0)
   (column
     :accessor column
     :initform 0)
   (fragments
     :accessor fragments
     :initform (make-array 32 :adjustable t :fill-pointer 0 :element-type 'fragment))
   (instructions
     :accessor instructions
     :initform (make-array 32 :adjustable t :fill-pointer 0 :element-type 'instruction))
   (blocks
     :accessor blocks
     :initform nil)))

(defmethod initialize-instance :after ((instance pretty-stream) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (slot-boundp instance 'target)
    (setf (column instance)
          (or (ignore-errors
                (trivial-gray-streams:stream-line-column (target instance)))
              0))))

(defun finalize-instructions (stream)
  (loop with client = (client stream)
        with instructions = (instructions stream)
        for instruction across instructions
        for i from 0
        for block-end = (find-if (lambda (x) (typep x 'block-end))
                                 instructions
                                 :from-end t
                                 :start i)
        when (and block-end
                  (typep instruction 'newline)
                  (not (section-end instruction)))
        do (setf (section-end instruction) block-end)
        when (typep instruction 'text)
        do (setf (value instruction) (normalize-text client stream (value instruction)))))

(defun layout-instructions (stream &aux (client (client stream))
                            (instructions (instructions stream)))
  (prog ((i 0) section previous success instruction single-line start-section-p)
   repeat
    (when (< i (length instructions))
      (setf instruction (aref instructions i)
            (break-column instruction) (if (zerop i)
                                         (column stream)
                                         (break-column (aref instructions (1- i))))
            (column instruction) (if (zerop i)
                                   (column stream)
                                   (column (aref instructions (1- i))))
            (line instruction) (if (zerop i)
                                 (line stream)
                                 (line (aref instructions (1- i))))
            (index instruction) (length (fragments stream)))
      (when (and (null section)
                 (typep instruction 'section-start))
        (setf section instruction
              single-line t
              previous nil))
      #+(or)(print section)
      (multiple-value-setq (success start-section-p)
                           (layout client stream instruction single-line))
      (cond
        ((and success start-section-p)
          (setf section instruction)
          (incf i))
        #+(or)((and success section (eq instruction (section-end section)))
          (setf section nil)
          (incf i))
        (success
          (when (typep instruction 'newline)
            (setf previous instruction))
          (incf i))
        ((null section)
          (error "Layout failure outside of a section."))
        (single-line
          (when previous
            (setf (single-line previous) nil))
          (setf i (position section instructions)
                single-line nil
                (fill-pointer (fragments stream)) (index section)))
        (previous
          (setf i (position previous instructions)
                (single-line previous) nil
                (fill-pointer (fragments stream)) (index previous)))
        (t
          (error "layout failure while in non-newline section in multiline mode.")))
      (go repeat)))
  (setf (fill-pointer (instructions stream)) 0))

(defun write-fragments (stream)
  (loop with client = (client stream)
        with fragments = (fragments stream)
        for i below (length (fragments stream))
        for fragment = (aref (fragments stream) i)
        for text = (text fragment)
        do (write-text client stream
                       (line fragment) (column fragment) text
                       0 (when (and text
                                    (< (1+ i) (length (fragments stream)))
                                    (null (text (aref (fragments stream) (1+ i)))))
                           (break-position client stream text))))
  (setf (fill-pointer (fragments stream)) 0))

(defun process-instructions (stream)
  (unless (blocks stream)
    (finalize-instructions stream)
    (layout-instructions stream)
    (write-fragments stream)))

(defmethod write-text (client (stream pretty-stream) line column text &optional start end)
  (when line
    (loop while (< (line stream) line)
          do (write-char #\Newline (target stream))
          do (incf (line stream)))
    (setf (column stream) 0))
  (when column
    (loop while (< (column stream) column)
          do (write-char #\Space (target stream))
          do (incf (column stream))))
  (when text
    (write-string text (target stream)
                  :start start
                  :end end)
    (incf (column stream) (text-width client stream text))))

(defgeneric layout (client stream instruction single-line))

(defun layout-arrange-text (client stream instruction single-line line column text)
  (let ((new-break-column (break-column instruction))
        (new-column (column instruction))
        (new-line (line instruction))
        (width (text-width client stream text))
        (break-width (text-width client stream text 0 (break-position client stream text))))
    (when line
      (setf new-line line
            new-break-column 0
            new-column 0))
    (when column
      (setf new-column column))
    (unless (zerop break-width)
      (setf new-break-column (+ new-column break-width)))
    (incf new-column width)
    #+(or)(format t "Single Line: ~A, Line: ~2d -> ~2d, Column: ~2d -> ~2d, Break Column: ~2d -> ~2d, Text: ~s~%"
                   single-line
                  (line instruction) new-line
                  (column instruction) new-column
                  (break-column instruction) new-break-column
                  text)
    (unless (and single-line
                 (< (right-margin client stream) new-break-column))
      (setf (break-column instruction) new-break-column
            (column instruction) new-column
            (line instruction) new-line)
      (vector-push-extend (make-instance 'fragment :line line :column column :text text)
                          (fragments stream))
      t)))

(defmethod layout (client stream (instruction text) single-line)
  (layout-arrange-text client stream instruction single-line nil nil (value instruction)))

(defmethod layout (client stream (instruction tab) single-line)
  (with-accessors ((column column)
                   (colnum colnum)
                   (colinc colinc))
                  instruction
    (layout-arrange-text client stream instruction single-line nil
                         (cond
                           ((< column colnum)
                             colnum)
                           ((not (zerop (colinc instruction)))
                             (+ colnum
                                (* colinc
                                   (floor (+ column (- colnum) colinc)
                                          colinc)))))
                         nil)))

(defmethod layout (client stream (instruction newline) single-line)
  (cond
    ((and single-line
          (member (kind instruction) '(:literal-mandatory :mandatory)))
      nil)
    ((or single-line
         (and (eq (kind instruction) :fill)
              (single-line instruction)))
      t)
    (t
      (layout-arrange-text client stream instruction single-line
                           (1+ (line instruction)) 0 nil)
      (unless (or (null (parent instruction))
                  (member (kind instruction) '(:literal-fill :literal-linear :literal-mandatory)))
        (map nil (lambda (fragment)
                   (vector-push-extend fragment (fragments stream)))
             (prefix-fragments (parent instruction)))
        (layout-arrange-text client stream instruction single-line
                             nil
                             (+ (start-column (parent instruction))
                                (indent (parent instruction)))
                             nil))
      (values t t))))

(defmethod layout (client stream (instruction indent) single-line)
  (setf (indent (parent instruction))
        (+ (width instruction)
           (ecase (kind instruction)
             (:block
               (start-column (parent instruction)))
             (:current
               (column instruction))))))

(defmethod layout (client stream (instruction block-start) single-line)
  (let* ((column (column instruction))
         (start-column (+ column
                          (text-width client stream
                                      (or (prefix instruction)
                                          (per-line-prefix instruction)
                                          ""))))
         (parent-prefix-fragments (when (parent instruction)
                                    (prefix-fragments (parent instruction))))
         (per-line-prefix (per-line-prefix instruction)))
    (setf (start-column instruction) start-column
          (section-column instruction) start-column
          (indent instruction) 0)
    (cond
      (parent-prefix-fragments
        (setf (prefix-fragments instruction)
              (make-array (1+ (length parent-prefix-fragments))
                          :fill-pointer (length parent-prefix-fragments)
                          :initial-contents parent-prefix-fragments
                          :element-type 'fragment))
        (vector-push (make-instance 'fragment :column column :text per-line-prefix)
                     (prefix-fragments instruction)))
      (per-line-prefix
        (setf (prefix-fragments instruction)
              (make-array 1 :element-type 'fragment
                          :initial-element (make-instance 'fragment :column column :text per-line-prefix)))))
    (layout-arrange-text client stream instruction single-line nil nil
                         (or (per-line-prefix instruction)
                             (prefix instruction)))))

(defmethod layout (client stream (instruction block-end) single-line)
  (layout-arrange-text client stream instruction single-line nil nil (suffix instruction)))

(defmethod pprint-newline (client kind (stream pretty-stream))
  (with-accessors ((instructions instructions))
                  stream
    (let* ((depth (length (blocks stream)))
           (newline (make-instance 'newline :kind kind :depth depth
                                   :parent (car (blocks stream)))))
      (loop for instruction across instructions
            when (and (typep instruction 'section-start)
                      ;(null (section-end instruction))
                      (<= (depth instruction) depth))
            do (setf (section-end instruction) newline))
      (vector-push-extend newline instructions)
      (process-instructions stream))))

(defmethod pprint-tab (client kind colnum colinc (stream pretty-stream))
  (vector-push-extend (make-instance 'tab :kind kind :colnum colnum :colinc colinc
                                     :parent (car (blocks stream)))
                      (instructions stream)))

(defmethod pprint-indent (client relative-to n (stream pretty-stream))
  (vector-push-extend (make-instance 'indent :kind relative-to :width n
                                     :parent (car (blocks stream)))
                      (instructions stream)))

(defmethod trivial-gray-streams:stream-file-position ((stream pretty-stream))
  (file-position (target stream)))

(defmethod trivial-gray-streams:stream-write-char ((stream pretty-stream) char)
  (if (char= char #\newline)
    (pprint-newline (client stream) :literal-mandatory stream)
    (pprint-text (client stream) stream char))
  char)

(defmethod pprint-text (client (stream pretty-stream) (text character) &optional start end)
  (declare (ignore start end))
  (with-accessors ((instructions instructions))
                  stream
    (let ((instruction (unless (zerop (length instructions)) (aref instructions (1- (length instructions))))))
      (if (typep instruction 'text)
        (setf (value instruction) (concatenate 'string (value instruction) (string text)))
        (vector-push-extend (make-instance 'text
                                           :value (string text)
                                           :parent (car (blocks stream)))
                            instructions)))))

(defmethod pprint-text (client (stream pretty-stream) (text string) &optional start end)
  (with-accessors ((instructions instructions))
                  stream
    (let ((instruction (unless (zerop (length instructions)) (aref instructions (1- (length instructions))))))
      (if (typep instruction 'text)
        (setf (value instruction)
              (concatenate 'string (value instruction) (subseq text start end)))
        (vector-push-extend (make-instance 'text
                                           :value (subseq text start end)
                                           :parent (car (blocks stream)))
                            instructions)))))

(defmethod trivial-gray-streams:stream-write-string ((stream pretty-stream) string &optional start end)
  (pprint-split (client stream) stream string start end))

(defmethod trivial-gray-streams:stream-finish-output ((stream pretty-stream))
  (process-instructions stream))

(defmethod trivial-gray-streams:stream-terpri ((stream pretty-stream))
  (pprint-newline (client stream) :literal-mandatory stream))

(defmethod trivial-gray-streams:stream-line-column ((stream pretty-stream))
  (column stream))

(defmethod make-pretty-stream ((client client) (stream pretty-stream))
  stream)

(defmethod make-pretty-stream ((client client) (stream (eql nil)))
  (make-instance 'pretty-stream :target *standard-output* :client client))

(defmethod make-pretty-stream ((client client) (stream (eql t)))
  (make-instance 'pretty-stream :target *terminal-io* :client client))

(defmethod make-pretty-stream ((client client) stream)
  (make-instance 'pretty-stream :target stream :client client))

(defmethod pprint-start-logical-block (client (stream pretty-stream) prefix per-line-prefix)
  (let ((block-start (make-instance 'block-start
                                    :prefix (normalize-text client stream prefix)
                                    :per-line-prefix (normalize-text client stream per-line-prefix)
                                    :depth (length (blocks stream))
                                    :parent (car (blocks stream)))))
    (push block-start (blocks stream))
    (vector-push-extend block-start (instructions stream))))

(defmethod pprint-end-logical-block (client (stream pretty-stream) suffix)
  (let ((block-end (make-instance 'block-end
                                  :suffix (normalize-text client stream suffix)
                                  :parent (car (blocks stream)))))
    (setf (block-end (car (blocks stream))) block-end)
    (pop (blocks stream))
    (vector-push-extend block-end (instructions stream))
    (process-instructions stream)))

