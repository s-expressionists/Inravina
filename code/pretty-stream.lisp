(in-package #:inravina)

(defclass instruction ()
  ((parent
    :initarg :parent
    :reader parent
    :type (or null block-start))
   (section
    :initarg :section
    :reader section
    :type (or null section-start))
   (fragment-index
    :initarg :fragment-index
    :initform nil
    :accessor fragment-index
    :type (or null integer))
   (instruction-index
    :initarg :instruction-index
    :initform nil
    :accessor instruction-index
    :type (or null integer))
   (line
    :initarg :line
    :initform nil
    :accessor line
    :type (or null integer))
   (break-column
    :initform nil
    :accessor break-column
    :type (or null real))
   (column
    :initarg :column
    :initform nil
    :accessor column
    :type (or null real))))

(defclass section-start (instruction)
  ((depth
    :initarg :depth
    :initform 0
    :reader depth
    :type integer)
   (section-end
    :initarg :section-end
    :initform nil
    :accessor section-end
    :type (or null newline block-end))))

(defclass text (instruction)
  ((value
    :initarg :value
    :initform ""
    :accessor value
    :type string)))

(defmethod print-object ((obj text) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (prin1 (value obj) stream)))

(defclass indent (instruction)
  ((kind
    :initarg :kind
    :reader kind
    :type (member :block :current))
   (width
    :initarg :width
    :reader width
    :type real)))

(defmethod print-object ((obj indent) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (prin1 (kind obj) stream)
     (write-char #\Space stream)
     (prin1 (width obj) stream)))

(defclass newline (section-start)
  ((kind
    :initarg :kind
    :reader kind
    :type newline-kind)))

(defmethod print-object ((obj newline) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (prin1 (kind obj) stream)))

(defclass tab (instruction)
  ((kind
    :initarg :kind
    :reader kind
    :type tab-kind)
   (colnum
    :initarg :colnum
    :reader colnum
    :type (or null real))
   (colinc
    :initarg :colinc
    :reader colinc
    :type (or null real))))

(defmethod print-object ((obj tab) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (prin1 (kind obj) stream)
     (write-char #\Space stream)
     (prin1 (colnum obj) stream)
     (write-char #\Space stream)
     (prin1 (colinc obj) stream)))

(defclass block-start (section-start)
  ((prefix
    :initarg :prefix
    :reader prefix
    :type (or null string))
   (per-line-prefix
    :initarg :per-line-prefix
    :reader per-line-prefix
    :type (or null string))
   (prefix-fragments
    :initform nil
    :accessor prefix-fragments)
   (start-column
    :initarg :start-column
    :initform 0
    :accessor start-column
    :type real)
   (indent
    :initarg :indent
    :initform nil
    :accessor indent
    :type (or null real))
   (block-end
    :initarg :block-end
    :initform nil
    :accessor block-end
    :type (or null block-end))))

(defclass block-end (instruction)
  ((suffix
    :initarg :suffix
    :accessor suffix
    :type (or null string))))

(defclass fragment ()
  ((line
    :initarg :line
    :initform nil
    :accessor line
    :type (or null integer))
   (column
    :initarg :column
    :initform nil
    :accessor column
    :type (or null real))
   (text
    :initarg :text
    :initform nil
    :accessor text
    :type (or null string))))

(defclass pretty-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((target
    :initarg :target
    :reader target)
   (client
    :initarg :client
    :reader client)
   (line
    :initform 0
    :accessor line
    :type real)
   (column
    :initform 0
    :accessor column
    :type real)
   (fragments
    :initform (make-array 32 :adjustable t :fill-pointer 0
                             :initial-element nil :element-type '(or null fragment))
    :reader fragments)
   (instructions
    :initform (make-array 32 :adjustable t :fill-pointer 0
                             :initial-element nil :element-type '(or null instruction))
    :reader instructions)
   (blocks
    :initform nil
    :accessor blocks
    :type list)
   (sections
    :initform nil
    :accessor sections
    :type list)))

(defmethod initialize-instance :after ((instance pretty-stream) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (slot-boundp instance 'target)
    (setf (column instance)
          (or (ignore-errors
                (trivial-gray-streams:stream-line-column (target instance)))
              0))))

(defmethod describe-object ((object pretty-stream) stream)
  (loop for instruction across (instructions object)
        do (typecase instruction
             (block-start (write-char #\< stream))
             (block-end (write-char #\> stream))
             (newline (write-char #\| stream))
             (text (dotimes (i (length (value instruction)))
                     (write-char #\- stream)))))
  (terpri stream)
  (loop for instruction across (instructions object)
        when (typep instruction 'section-start)
        do (progn (loop with ch = #\Space
                 for sub across (instructions object)
                 do (cond ((eq sub instruction)
                            (write-char #\[ stream)
                            (setf ch #\-))
                          ((eq sub (section-end instruction))
                            (write-char #\] stream)
                            (setf ch #\Space))
                          ((typep sub '(or block-start block-end newline))
                            (write-char ch stream))
                          ((typep sub 'text)
                            (dotimes (i (length (value sub)))
                              (write-char ch stream)))))
        (terpri stream))))

(defun layout-instructions (stream)
  (prog ((index 0) (section t) last-maybe-break status instruction mode
         (client (client stream))
         (instructions (instructions stream)))
   repeat
    (when (< index (length instructions))
      (setf instruction (aref instructions index)
            status (layout client stream mode instruction
                           (unless (zerop index)
                             (aref instructions (1- index)))
                           (or (not section)
                               (and (typep section 'section-start)
                                    (or (eq section instruction)
                                        (eq (section-end section) instruction)))))
            mode (and (eq :overflow mode) mode))
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
        (:overflow
         (setf mode :overflow)
         (incf index))
        (otherwise
         (cond
           (section
            (setf index (if (eq t section)
                            0
                            (1+ (instruction-index section)))
                  section nil
                  (fill-pointer (fragments stream)) (fragment-index (aref instructions index))))
           (last-maybe-break
            (setf index (instruction-index last-maybe-break)
                  (fill-pointer (fragments stream)) (fragment-index last-maybe-break)
                  last-maybe-break nil
                  mode t))
           (t
            (setf mode t)))))
      (go repeat)))
  (setf (fill-pointer (instructions stream)) 0))

(defun positioning-fragment (fragment &aux (text (text fragment)))
  (and (or (line fragment)
           (column fragment))
       (or (null text)
           (zerop (length text)))))

(defun write-fragments (stream)
  (loop with client = (client stream)
        with fragments = (fragments stream)
        for i below (length (fragments stream))
        for fragment = (aref (fragments stream) i)
        for text = (text fragment)
        unless (and (not text)
                    (not (line fragment))
                    (or (not (column fragment))
                        (and (< (1+ i) (length (fragments stream)))
                             (positioning-fragment (aref (fragments stream) (1+ i))))))
        do (write-text client stream
                       (line fragment) (column fragment) text
                       0 (when (and text
                                    (< (1+ i) (length (fragments stream)))
                                    (positioning-fragment (aref (fragments stream) (1+ i))))
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
          do (incf (column stream) (text-width client stream #\Space))))
  (when text
    (write-string text (target stream)
                  :start start
                  :end end)
    (incf (column stream) (text-width client stream text start end))))

(defgeneric layout (client stream mode instruction previous-instruction allow-break-p)
  (:method (client stream (mode (eql :overflow)) instruction previous-instruction allow-break-p)
    (declare (ignore client stream mode instruction previous-instruction allow-break-p))
    t))

(defmethod layout :before
    (client stream mode instruction (previous-instruction (eql nil)) allow-break-p)
  (declare (ignore client allow-break-p mode))
  (setf (break-column instruction) (column stream)
        (column instruction) (column stream)
        (line instruction) (line stream)
        (fragment-index instruction) (length (fragments stream))))

(defmethod layout :before
    (client stream mode instruction (previous-instruction instruction) allow-break-p)
  (declare (ignore client allow-break-p mode))
  (setf (break-column instruction) (break-column previous-instruction)
        (column instruction) (column previous-instruction)
        (line instruction) (line previous-instruction)
        (fragment-index instruction) (length (fragments stream))))

(defun add-fragment (client stream mode instruction line column text)
  (unless (or line
              column
              (and text
                   (not (zerop (length text)))))
    (return-from add-fragment t))
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
    (when (or mode
              (>= (right-margin client stream) new-break-column))
      (setf (break-column instruction) new-break-column
            (column instruction) new-column
            (line instruction) new-line)
      (vector-push-extend (make-instance 'fragment :line line :column column :text text)
                          (fragments stream))
      t)))

(defmethod layout (client stream mode (instruction text) previous-instruction allow-break-p)
  (declare (ignore previous-instruction allow-break-p))
  (add-fragment client stream mode instruction nil nil (value instruction)))

(defun compute-tab-size (column colnum colinc relativep)
  (cond (relativep
         (unless (<= colinc 1)
           (let ((newposn (+ column colnum)))
             (let ((rem (rem newposn colinc)))
               (unless (zerop rem)
                 (incf colnum (- colinc rem))))))
         colnum)
        ((< column colnum)
         (- colnum column))
        ((= column colnum)
         colinc)
        ((plusp colinc)
         (- colinc (rem (- column colnum) colinc)))
        (t
         0)))

(defmethod layout (client stream mode (instruction tab) previous-instruction allow-break-p)
  (declare (ignore previous-instruction allow-break-p))
  (let* ((section-column (if (and (section instruction)
                                  (section-kind-p (kind instruction)))
                           (column (section instruction))
                           0))
         (column (- (column instruction) section-column)))
    (add-fragment client stream mode instruction nil
                  (+ section-column
                     column
                     (compute-tab-size column
                                       (colnum instruction)
                                       (colinc instruction)
                                       (relative-kind-p (kind instruction))))
                  nil)))

(defmethod layout (client stream mode (instruction newline) previous-instruction allow-break-p)
  (cond ((and (not allow-break-p)
              (or (mandatory-kind-p (kind instruction))
                  (and (miser-kind-p (kind instruction))
                       (miser-p client stream))))
         nil)
        ((and (not mode)
              (not allow-break-p))
         t)
        ((and (not mode)
              (or (fill-kind-p (kind instruction))
                  (and (miser-kind-p (kind instruction))
                       (not (miser-p client stream)))))
         :maybe-break)
        ((equal (1+ (line instruction)) *print-lines*)
         (add-fragment client stream mode instruction nil nil "..")
         :overflow)
        (t
         (add-fragment client stream mode instruction (1+ (line instruction)) 0 nil)
         (unless (or (null (parent instruction))
                     (literal-kind-p (kind instruction)))
           (map nil (lambda (fragment)
                      (vector-push-extend fragment (fragments stream)))
                (prefix-fragments (parent instruction)))
           (add-fragment client stream mode instruction
                                nil
                                (+ (start-column (parent instruction))
                                   (indent (parent instruction)))
                                nil))
         :break)))

(defmethod layout (client stream mode (instruction indent) previous-instruction allow-break-p)
  (declare (ignore client stream previous-instruction allow-break-p mode))
  (setf (indent (parent instruction))
        (ecase (kind instruction)
          (:block
           (width instruction))
          (:current
           (+ (width instruction)
              (column instruction)
              (- (start-column (parent instruction)))))))
  t)

(defmethod layout (client stream (mode (eql :overflow)) (instruction block-start) previous-instruction allow-break-p)
  (setf (suffix (block-end instruction)) "")
  t)

(defmethod layout (client stream mode (instruction block-start) previous-instruction allow-break-p)
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
          (indent instruction) 0)
    (cond (parent-prefix-fragments
           (setf (prefix-fragments instruction)
                 (make-array (1+ (length parent-prefix-fragments))
                             :fill-pointer (length parent-prefix-fragments)
                             :initial-contents parent-prefix-fragments
                             :element-type 'fragment))
           (vector-push (make-instance 'fragment :column column :text per-line-prefix)
                        (prefix-fragments instruction)))
          (per-line-prefix
           (setf (prefix-fragments instruction)
                 (make-array 1
                             :element-type 'fragment
                             :initial-element (make-instance 'fragment :column column :text per-line-prefix)))))
    (add-fragment client stream mode instruction nil nil
                  (or (per-line-prefix instruction)
                      (prefix instruction)))))

(defmethod layout (client stream (mode (eql :overflow)) (instruction block-end) previous-instruction allow-break-p)
  (add-fragment client stream mode instruction nil nil (suffix instruction)))

(defmethod layout (client stream mode (instruction block-end) previous-instruction allow-break-p)
  (add-fragment client stream mode instruction nil nil (suffix instruction)))

(defmethod pprint-newline (client (stream pretty-stream) kind)
  (with-accessors ((instructions instructions)
                   (sections sections))
                  stream
    (let* ((depth (length (blocks stream)))
           (newline (make-instance 'newline
                                   :kind kind :depth depth
                                   :section (car (sections stream))
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
      (when (and sections
                 (eq (section-end (car sections)) newline))
        (pop sections))
      (push newline sections)
      (vector-push-extend newline instructions)
      (process-instructions stream))))

(defmethod pprint-tab (client (stream pretty-stream) kind colnum colinc)
  (vector-push-extend (make-instance 'tab
                                     :kind kind :colnum colnum :colinc colinc
                                     :section (car (sections stream))
                                     :instruction-index (length (instructions stream))
                                     :parent (car (blocks stream)))
                      (instructions stream)))

(defmethod pprint-indent (client (stream pretty-stream) relative-to n)
  (vector-push-extend (make-instance 'indent
                                     :kind relative-to :width n
                                     :section (car (sections stream))
                                     :instruction-index (length (instructions stream))
                                     :parent (car (blocks stream)))
                      (instructions stream)))

(defmethod trivial-gray-streams:stream-file-position ((stream pretty-stream))
  (file-position (target stream)))

(defmethod trivial-gray-streams:stream-write-char ((stream pretty-stream) char)
  (if (char= char #\newline)
      (pprint-newline (client stream) stream :literal-mandatory)
      (pprint-text (client stream) stream char))
  char)

(defmethod pprint-text (client (stream pretty-stream) (text character) &optional start end)
  (declare (ignore start end))
  (with-accessors ((instructions instructions))
                  stream
    (let ((instruction (and (plusp (length instructions))
                            (aref instructions (1- (length instructions))))))
      (if (typep instruction 'text)
          (setf (value instruction) (concatenate 'string (value instruction) (string text)))
          (vector-push-extend (make-instance 'text
                                             :value (string text)
                                             :section (car (sections stream))
                                             :instruction-index (length instructions)
                                             :parent (car (blocks stream)))
                              instructions)))))

(defmethod pprint-text (client (stream pretty-stream) (text string) &optional start end)
  (with-accessors ((instructions instructions))
                  stream
    (let ((instruction (and (plusp (length instructions))
                            (aref instructions (1- (length instructions))))))
      (if (typep instruction 'text)
          (setf (value instruction)
                (concatenate 'string (value instruction) (subseq text start end)))
          (vector-push-extend (make-instance 'text
                                             :section (car (sections stream))
                                             :instruction-index (length instructions)
                                             :value (subseq text start end)
                                             :parent (car (blocks stream)))
                              instructions)))))

(defmethod trivial-gray-streams:stream-write-string ((stream pretty-stream) string &optional start end)
  (pprint-split (client stream) stream string start end))

(defmethod trivial-gray-streams:stream-finish-output ((stream pretty-stream))
  (process-instructions stream))

(defmethod trivial-gray-streams:stream-terpri ((stream pretty-stream))
  (pprint-newline (client stream) stream :literal-mandatory))

(defmethod trivial-gray-streams:stream-line-column ((stream pretty-stream))
  (column stream))

(defmethod make-pretty-stream ((client client) (stream broadcast-stream))
  (if (broadcast-stream-streams stream)
      (call-next-method)
      stream))

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
                                    :section (car (sections stream))
                                    :instruction-index (length (instructions stream))
                                    :prefix (normalize-text client stream prefix)
                                    :per-line-prefix (normalize-text client stream per-line-prefix)
                                    :depth (1+ (length (blocks stream)))
                                    :parent (car (blocks stream)))))
    (push block-start (blocks stream))
    (push block-start (sections stream))
    (vector-push-extend block-start (instructions stream))))

(defmethod pprint-end-logical-block (client (stream pretty-stream) suffix)
  (let ((block-end (make-instance 'block-end
                                  :section (car (sections stream))
                                  :instruction-index (length (instructions stream))
                                  :suffix (normalize-text client stream suffix)
                                  :parent (car (blocks stream)))))
    (setf (block-end (car (blocks stream))) block-end)
    (pop (blocks stream))
    (vector-push-extend block-end (instructions stream))
    (process-instructions stream)))

(defmethod miser-p (client (stream pretty-stream))
  (and *print-miser-width*
       (<= (- (right-margin client stream)
              (column stream))
           *print-miser-width*)))


