(in-package #:inravina)

(defclass instruction ()
  ((parent
     :accessor parent
     :initarg :parent)
   (fragment-index
     :accessor fragment-index
     :initarg :fragment-index
     :initform nil
     :type (or null integer))
   (instruction-index
     :accessor instruction-index
     :initarg :instruction-index
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
;(inravina:pprint-bindings inravina:*client* nil '((fu 1) (bar 2)) t)
(defmethod print-object ((obj text) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (prin1 (value obj) stream)))

(defclass indent (instruction)
  ((kind
     :accessor kind
     :initarg :kind
     :type (member :block :current))
   (width
     :accessor width
     :initarg :width)))

(defmethod print-object ((obj indent) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (prin1 (kind obj) stream)
     (write-char #\Space stream)
     (prin1 (width obj) stream)))

(defclass newline (section-start)
  ((kind
     :accessor kind
     :initarg :kind
     :type newline-kind)))

(defmethod print-object ((obj newline) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (prin1 (kind obj) stream)))

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

(defmethod print-object ((obj tab) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (prin1 (kind obj) stream)
     (write-char #\Space stream)
     (prin1 (colnum obj) stream)
     (write-char #\Space stream)
     (prin1 (colinc obj) stream)))

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

(defun print-instructions (stream)
  (loop for instruction across (instructions stream)
        do (typecase instruction
             (block-start (write-char #\< *debug-io*))
             (block-end (write-char #\> *debug-io*))
             (newline (write-char #\| *debug-io*))
             (text (dotimes (i (length (value instruction)))
                     (write-char #\- *debug-io*)))))
  (terpri *debug-io*)
  (loop for instruction across (instructions stream)
        when (typep instruction 'section-start)
        do (progn (loop with ch = #\Space
                 for sub across (instructions stream)
                 do (cond ((eq sub instruction)
                            (write-char #\[ *debug-io*)
                            (setf ch #\-))
                          ((eq sub (section-end instruction))
                            (write-char #\] *debug-io*)
                            (setf ch #\Space))
                          ((typep sub '(or block-start block-end newline))
                            (write-char ch *debug-io*))
                          ((typep sub 'text)
                            (dotimes (i (length (value sub)))
                              (write-char ch *debug-io*)))))
        (terpri *debug-io*))))


(defun layout-instructions (stream &aux (client (client stream))
                            (instructions (instructions stream)))
  (prog ((index 0) (section t) last-maybe-break status instruction margin-release-p)
   repeat
    (when (< index (length instructions))
      (setf instruction (aref instructions index)
            (break-column instruction) (if (zerop index)
                                           (column stream)
                                           (break-column (aref instructions (1- index))))
            (column instruction) (if (zerop index)
                                     (column stream)
                                     (column (aref instructions (1- index))))
            (line instruction) (if (zerop index)
                                   (line stream)
                                   (line (aref instructions (1- index))))
            (fragment-index instruction) (length (fragments stream)))
      (setf status (layout client stream instruction
                           (and section
                                (or (not (typep section 'section-start))
                                    (and (not (eq section instruction))
                                         (not (eq (section-end section) instruction)))))
                           margin-release-p)
            margin-release-p nil)
      (case status
        ((t :maybe-break)
          (cond ((and (null section)
                      (typep instruction 'section-start))
                 (setf section instruction))
                ((or (eq section instruction)
                     (and (typep section 'section-start)
                          (eq instruction (section-end section))))
                 (setf section nil)))
          (when (eq status :maybe-break)
            (setf last-maybe-break instruction))
          (incf index))
        (:break
          (setf section (and (not (eq section instruction))
                             instruction)
                last-maybe-break nil)
          (incf index))
        (otherwise
          (cond
            ((eq t section)
             (setf index 0
                   section nil
                   (fill-pointer (fragments stream)) (fragment-index (aref instructions 0))))
            (section
             (setf index (instruction-index section)
                   (fill-pointer (fragments stream)) (fragment-index section)))
            (last-maybe-break
             (setf index (instruction-index last-maybe-break)
                   (fill-pointer (fragments stream)) (fragment-index last-maybe-break)
                   last-maybe-break nil
                   margin-release-p t))
            (t
             (setf margin-release-p t)))))
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

(defgeneric layout (client stream instruction single-line-p margin-release-p))

(defun layout-arrange-text (client stream instruction single-line-p margin-release-p line column text)
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
    (when (or margin-release-p
              (>= (right-margin client stream) new-break-column))
      (setf (break-column instruction) new-break-column
            (column instruction) new-column
            (line instruction) new-line)
      (vector-push-extend (make-instance 'fragment :line line :column column :text text)
                          (fragments stream))
      t)))

(defmethod layout (client stream (instruction text) single-line-p margin-release-p)
  (layout-arrange-text client stream instruction single-line-p margin-release-p nil nil (value instruction)))

(defmethod layout (client stream (instruction tab) single-line-p margin-release-p)
  (with-accessors ((column column)
                   (colnum colnum)
                   (colinc colinc))
                  instruction
    (layout-arrange-text client stream instruction single-line-p margin-release-p nil
                         (cond
                           ((< column colnum)
                             colnum)
                           ((not (zerop (colinc instruction)))
                             (+ colnum
                                (* colinc
                                   (floor (+ column (- colnum) colinc)
                                          colinc)))))
                         nil)))

(defmethod layout (client stream (instruction newline) single-line-p margin-release-p)
  (cond
    ((and single-line-p
          (or (mandatory-kind-p (kind instruction))
              (and (miser-kind-p (kind instruction))
                   (miser-p client stream))))
      nil)
    ((and (not margin-release-p)
          single-line-p)
      t)
    ((and (not margin-release-p)
          (or (fill-kind-p (kind instruction))
              (and (miser-kind-p (kind instruction))
                   (not (miser-p client stream)))))
      :maybe-break)
    (t
      (layout-arrange-text client stream instruction single-line-p margin-release-p
                           (1+ (line instruction)) 0 nil)
      (unless (or (null (parent instruction))
                  (literal-kind-p (kind instruction)))
        (map nil (lambda (fragment)
                   (vector-push-extend fragment (fragments stream)))
             (prefix-fragments (parent instruction)))
        (layout-arrange-text client stream instruction single-line-p margin-release-p
                             nil
                             (+ (start-column (parent instruction))
                                (indent (parent instruction)))
                             nil))
      :break)))

(defmethod layout (client stream (instruction indent) single-line-p margin-release-p)
  (setf (indent (parent instruction))
        (ecase (kind instruction)
          (:block
            (width instruction))
          (:current
            (+ (width instruction)
               (column instruction)
               (- (start-column (parent instruction)))))))
  t)

(defmethod layout (client stream (instruction block-start) single-line-p margin-release-p)
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
    (layout-arrange-text client stream instruction single-line-p margin-release-p nil nil
                         (or (per-line-prefix instruction)
                             (prefix instruction)))))

(defmethod layout (client stream (instruction block-end) single-line-p margin-release-p)
  (layout-arrange-text client stream instruction single-line-p margin-release-p nil nil (suffix instruction)))

(defmethod pprint-newline (client kind (stream pretty-stream))
  (with-accessors ((instructions instructions))
                  stream
    (let* ((depth (length (blocks stream)))
           (newline (make-instance 'newline :kind kind :depth depth
                                   :instruction-index (length instructions)
                                   :parent (car (blocks stream))))
           (section-start (find-if (lambda (ins)
                                     (and (typep ins 'section-start)
                                          (null (section-end ins))
                                          (= (depth ins) depth)))
                                   instructions
                                   :from-end t)))
      (when section-start
        (setf (section-end section-start) newline))
      (loop for ins across instructions
            when (and (typep ins 'newline)
                      (null (section-end ins))
                      (> (depth ins) depth))
            do (setf (section-end ins) newline))
      (vector-push-extend newline instructions)
      (process-instructions stream))))

(defmethod pprint-tab (client kind colnum colinc (stream pretty-stream))
  (vector-push-extend (make-instance 'tab :kind kind :colnum colnum :colinc colinc
                                     :instruction-index (length (instructions stream))
                                     :parent (car (blocks stream)))
                      (instructions stream)))

(defmethod pprint-indent (client relative-to n (stream pretty-stream))
  (vector-push-extend (make-instance 'indent :kind relative-to :width n
                                     :instruction-index (length (instructions stream))
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
                                           :instruction-index (length instructions)
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
                                           :instruction-index (length instructions)
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
                                    :instruction-index (length (instructions stream))
                                    :prefix (normalize-text client stream prefix)
                                    :per-line-prefix (normalize-text client stream per-line-prefix)
                                    :depth (1+ (length (blocks stream)))
                                    :parent (car (blocks stream)))))
    (push block-start (blocks stream))
    (vector-push-extend block-start (instructions stream))))

(defmethod pprint-end-logical-block (client (stream pretty-stream) suffix)
  (let ((block-end (make-instance 'block-end
                                  :instruction-index (length (instructions stream))
                                  :suffix (normalize-text client stream suffix)
                                  :parent (car (blocks stream))))
        (depth (length (blocks stream))))
    (setf (block-end (car (blocks stream))) block-end)
    (pop (blocks stream))
    (vector-push-extend block-end (instructions stream))
    (process-instructions stream)))

(defmethod miser-p (client (stream pretty-stream))
  (and *print-miser-width*
       (<= (- (right-margin client stream)
              (column stream))
           *print-miser-width*)))


