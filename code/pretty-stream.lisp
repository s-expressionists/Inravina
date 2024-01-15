(in-package #:inravina)

(defclass instruction ()
  ((parent :accessor parent
           :initarg :parent
           :type (or null block-start))
   (next :accessor next
         :initarg :next
         :initform nil
         :type (or null instruction))
   (previous :accessor previous
             :initarg :previous
             :initform nil
             :type (or null instruction))
   (section :accessor section
            :initarg :section
            :type (or null section-start))
   (fragment-index :accessor fragment-index
                   :initarg :fragment-index
                   :type integer)
   (line :accessor line
         :initarg :line
         :initform nil
         :type (or null integer))
   (column :accessor column
           :initarg :column
           :initform nil
           :type (or null real))
   (style :accessor style
          :initarg :style
          :initform nil)))

(defclass section-start (instruction)
  ((depth :accessor depth
          :initarg :depth
          :initform 0
          :type integer)
   (section-end :accessor section-end
                :initarg :section-end
                :initform nil
                :type (or null newline block-end))))

(defclass text (instruction)
  ((value :accessor value
          :initarg :value
          :initform (make-array 32 :adjustable t :fill-pointer 0 :element-type 'character) 
          :type string)))

(defclass advance (instruction)
  ((value :accessor value
          :initarg :value
          :initform 0    
          :type real)))

(defclass style (instruction)
  ())

(defmethod print-object ((obj text) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (prin1 (value obj) stream)))

(defclass indent (instruction)
  ((width :reader width
          :initarg :width    
          :type real)))

(defclass block-indent (indent)
  ())

(defclass current-indent (indent)
  ())

(defmethod print-object ((obj indent) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (prin1 (width obj) stream)))

(defclass newline (section-start)
  ((break-before-p :accessor break-before-p
                   :initarg :break-before-P
                   :initform nil
                   :type boolean)
   (newlines-after :accessor newlines-after
                   :initform nil
                   :type list)))

(defclass fresh-newline (newline)
  ())

(defclass mandatory-newline (newline)
  ())

(defclass fill-newline (newline)
  ())

(defclass linear-newline (newline)
  ())

(defclass miser-newline (newline)
  ())

(defclass literal-newline (newline)
  ())

(defclass fresh-literal-newline (literal-newline fresh-newline)
  ())

(defclass mandatory-literal-newline (literal-newline mandatory-newline)
  ())

(defclass fill-literal-newline (literal-newline fill-newline)
  ())

(defclass linear-literal-newline (literal-newline linear-newline)
  ())

(defclass miser-literal-newline (literal-newline miser-newline)
  ())

(defmethod print-object ((obj newline) stream)
  (print-unreadable-object (obj stream :type t :identity t)))

(defclass tab (instruction)
  ((colnum :reader colnum
           :initarg :colnum    
           :type (or null real))
   (colinc :reader colinc
           :initarg :colinc    
           :type (or null real))))

(defclass line-tab (tab)
  ())

(defclass section-tab (tab)
  ())

(defclass relative-tab (tab)
  ())

(defclass line-relative-tab (relative-tab line-tab)
  ())

(defclass section-relative-tab (relative-tab section-tab)
  ())

(defmethod print-object ((obj tab) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (prin1 (colnum obj) stream)
    (write-char #\Space stream)
    (prin1 (colinc obj) stream)))

(defclass block-start (section-start)
  ((prefix :reader prefix
           :initarg :prefix    
           :type string)
   (per-line-prefix-p :reader per-line-prefix-p
                      :initarg :per-line-prefix-p
                      :type boolean)
   (prefix-fragments :accessor prefix-fragments
                     :initform nil)
   (start-column :accessor start-column
                 :initarg :start-column
                 :initform 0    
                 :type real)
   (indent :accessor indent
           :initarg :indent
           :initform nil    
           :type (or null real))
   (block-end :accessor block-end
              :initarg :block-end
              :initform nil    
              :type (or null block-end))))

(defclass block-end (instruction)
  ((suffix :accessor suffix
           :initarg :suffix    
           :type string)))

(defclass pretty-stream
    (ngray:fundamental-character-output-stream)
  ((target :reader target
           :initarg :target)
   (client :reader client
           :initarg :client)
   (fragments :reader fragments
              :initform (make-array 256 :adjustable t
                                    :fill-pointer 0
                                    :element-type '(or function string real)))
   (head :accessor head
         :initform nil
         :type (or null instruction))
   (tail :accessor tail
         :initform nil
         :type (or null instruction))
   (blocks :accessor blocks
           :initform nil    
           :type list)
   (sections :accessor sections
             :initform nil
             :type list)))

(defparameter *debug-instruction* nil)

(defparameter *debug-section* nil)

(defparameter *pprint-debug* nil)

(defmethod describe-object ((object pretty-stream) stream)
  (when *debug-instruction*
    (loop for instruction = (head object) then (next instruction)
          for char = (if (eq instruction *debug-instruction*)
                             #\*
                             #\Space)
          finally (terpri stream)
          while instruction
          if (typep instruction 'text)
            do (dotimes (i (length (value instruction))) (write-char char stream))
          else
            do (write-char char stream)))
  (loop for instruction = (head object) then (next instruction)
        while instruction
        do (typecase instruction
             (block-start (write-char #\< stream))
             (block-end (write-char #\> stream))
             (fresh-newline (write-char #\r stream))
             (fresh-literal-newline (write-char #\R stream))
             (mandatory-newline (write-char #\x stream))
             (mandatory-literal-newline (write-char #\x stream))
             (linear-newline (write-char #\l stream))
             (linear-literal-newline (write-char #\L stream))
             (fill-newline (write-char #\f stream))
             (fill-literal-newline (write-char #\F stream))
             (miser-newline (write-char #\m stream))
             (miser-literal-newline (write-char #\M stream))
             (block-indent (write-char #\I stream))
             (current-indent (write-char #\i stream))
             (advance (write-char #\a stream))
             (line-tab (write-char #\t stream))
             (line-relative-tab (write-char #\T stream))
             (section-tab (write-char #\u stream))
             (section-relative-tab (write-char #\U stream))
             (style (write-char #\s stream))
             (text (dotimes (i (length (value instruction)))
                     (write-char #\- stream)))
             (otherwise (write-char #\? stream))))
  (terpri stream)
  (loop for instruction = (head object) then (next instruction)
        while instruction
        when (and (typep instruction 'section-start)
                  (or (not *debug-instruction*)
                      (eq *debug-section* instruction)))
          do (loop with ch = #\Space
                   for sub = (head object) then (next sub)
                   while sub
                   finally (terpri stream)
                   if (eq sub instruction)
                     do (write-char #\[ stream)
                        (setf ch #\-)
                   else if (eq sub (section-end instruction))
                     do (write-char #\] stream)
                        (setf ch #\Space)
                   else if (typep sub '(or block-start block-end newline))
                     do (write-char ch stream)
                   else if (typep sub 'text)
                     do (dotimes (i (length (value sub)))
                          (write-char ch stream))
                   else
                     do (write-char ch stream))))

(defun line-length (stream)
  (or *print-right-margin*
      #+gray-streams-line-length (ngray:stream-line-length (target stream))
      100))

(defun ancestor-p (instruction ancestor)
  (loop for parent = (parent instruction) then (parent parent)
        if (eq parent ancestor)
          return t
        else if (null parent)
          return nil))

(defun layout-instructions (stream)
  #+pprint-debug (when *pprint-debug*
                   (describe stream *debug-io*))
  (prog ((section t)
         last-maybe-break
         status
         (mode :single-line)
         (client (client stream))
         (instruction (head stream)))
   repeat
     (when instruction
       #+pprint-debug (when *pprint-debug*
                        (let ((*debug-instruction* instruction)
                              (*debug-section* section))
                          (describe stream *debug-io*)
                          (finish-output *debug-io*)
                          (format *debug-io* "section ~a, instruction ~a, mode = ~s~%"
                                  section
                                  instruction mode)))
       (setf status (layout client stream mode instruction))
       #+pprint-debug
       (when *pprint-debug*
         (format *debug-io* "status = ~a~%"
                 status))
       (cond ((eq mode :overflow-lines)
              (setf instruction (next instruction)))
             ((member status '(:no-break :maybe-break))
              (cond ((and (or (null section)
                              (and (typep section 'section-start)
                                   (eq instruction (section-end section))))
                          (typep instruction 'section-start))
                     (setf section instruction))
                    ((or (eq section instruction)
                         (and (typep section 'section-start)
                              (eq instruction (section-end section))))
                     (setf section nil)))
              (when (and (eq status :maybe-break)
                         (or (null last-maybe-break)
                             (ancestor-p last-maybe-break (parent instruction))))
                (setf last-maybe-break instruction))
              (setf instruction (next instruction)
                    mode (if (or (not section)
                                 (and (typep section 'section-start)
                                      (or (eq section instruction)
                                          (eq (section-end section) instruction))))
                             :multiline
                             :single-line)))
             ((eq status :break)
              (loop for i in (newlines-after instruction)
                    do (setf (break-before-p i) t))
              (setf section (and (not (eq section instruction))
                                 instruction)
                    mode (if section :single-line :multiline)
                    last-maybe-break nil
                    instruction (next instruction)))
             ((eq status :overflow-lines)
              (setf mode :overflow-lines
                    section nil
                    instruction (next instruction)))
             (last-maybe-break
              (setf instruction last-maybe-break
                    (fill-pointer (fragments stream)) (fragment-index last-maybe-break)
                    section last-maybe-break
                    last-maybe-break nil
                    mode :unconditional))
             (section
              (setf instruction (if (eq t section)
                                    (head stream)
                                    (next section))
                    section nil
                    mode :multiline
                    (fill-pointer (fragments stream)) (fragment-index instruction)))
             (t
              (setf mode :unconditional)))
       (go repeat)))
  (setf (head stream) nil
        (tail stream) nil))
        
(defun text-before-newline-p (stream index)
  (loop with fragments = (fragments stream)
        for i from (1+ index) below (length fragments)
        for fragment = (aref fragments i)
        if (typep fragment 'string)
          return t
        else if (eq #'terpri fragment)
          return nil
        finally (return t)))

(defun write-fragments (stream)
  (loop with client = (client stream)
        with target = (target stream)
        with fragments = (fragments stream)
        for fragment across fragments
        for i from 0
        do (etypecase fragment
             (function
              (funcall fragment target))
             (real
              (when (and (text-before-newline-p stream i)
                         (plusp fragment))
                (ngray:stream-advance-to-column target fragment)))
             (string
              (write-string fragment target
                            :start 0
                            :end (unless (text-before-newline-p stream i)
                                   (break-position client stream fragment)))))
        finally (finish-output target)
                (setf (fill-pointer fragments) 0)))

(defun process-instructions (stream)
  (unless (blocks stream)
    (layout-instructions stream)
    (write-fragments stream)))

(defgeneric layout (client stream mode instruction)
  (:method (client stream (mode (eql :overflow-lines)) instruction)
    (declare (ignore client stream mode instruction))
    :no-break))

(defmethod layout :before (client stream mode instruction
                           &aux (previous (previous instruction)))
  (declare (ignore client mode))
  (if previous
      (setf (column instruction)
            (stream-scale-column (target stream) (column previous)
                                 (style previous) (style instruction))
            (line instruction) (line previous))
      (setf (column instruction)
            (stream-scale-column (target stream)
                                 (or (ngray:stream-line-column (target stream)) 0)
                                 (style instruction) nil)
            (line instruction) 0))
  (setf (fragment-index instruction) (length (fragments stream))))

(defun add-newline-fragment (client stream mode instruction)
  (declare (ignore client mode))
  (vector-push-extend #'terpri (fragments stream))
  (setf (column instruction) 0)
  (incf (line instruction))
  :no-break)

(defun add-tab-fragment (client stream mode instruction column)
  (declare (ignore client mode))
  (vector-push-extend column (fragments stream))
  (setf (column instruction) column)
  :no-break)

(defun add-style-fragment (client stream mode instruction style)
  (declare (ignore client mode instruction))
  (vector-push-extend (lambda (stream)
                        (setf (stream-style stream) style))
                      (fragments stream))
  :no-break)

(defun add-text-fragment (client stream mode instruction text)
  (declare (ignore client))
  (if (or (null text)
          (zerop (length text)))
      :no-break
      (let ((new-column (+ (column instruction)
                           (stream-measure-string (target stream) text
                                                  (style instruction)))))
        (when (or (member mode '(:unconditional :overflow-lines))
                  (>= (line-length stream) new-column))
          (setf (column instruction) new-column)
          (vector-push-extend text (fragments stream))
          :no-break))))

(defmethod layout (client stream mode (instruction advance))
  (add-tab-fragment client stream mode instruction (value instruction)))

(defmethod layout (client stream mode (instruction text))
  (add-text-fragment client stream mode instruction (value instruction)))

(defmethod layout (client stream mode (instruction style))
  (add-style-fragment client stream mode instruction (style instruction)))

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

(defmethod layout (client stream mode (instruction section-tab)
                   &aux (column (column instruction)))
  (add-tab-fragment client stream mode instruction
                    (+ column
                       (compute-tab-size (- column
                                            (if (section instruction)
                                                (column (section instruction))
                                                0))
                                         (colnum instruction)
                                         (colinc instruction)
                                         (typep instruction 'relative-tab)))))

(defmethod layout (client stream mode (instruction line-tab)
                   &aux (column (column instruction)))
  (add-tab-fragment client stream mode instruction
                    (+ column
                       (compute-tab-size column
                                         (colnum instruction)
                                         (colinc instruction)
                                         (typep instruction 'relative-tab)))))

(defun miser-p (stream instruction
                &aux (line-length (line-length stream))
                     (line-column (column (parent instruction))))
  (and *print-miser-width*
       (and line-length
            line-column
            (<= (- line-length line-column)
                *print-miser-width*))))

(defmethod layout
    (client stream (mode (eql :single-line)) (instruction mandatory-newline))
  (declare (ignore client stream))
  nil)

(defmethod layout
    (client stream (mode (eql :multiline)) (instruction miser-newline))
  (declare (ignore client))
  (if (miser-p stream instruction)
      (call-next-method)
      :no-break))

(defmethod layout
    (client stream (mode (eql :multiline)) (instruction fresh-newline))
  (declare (ignore client stream))
  (if (zerop (column instruction))
      :no-break
      (call-next-method)))

(defmethod layout (client stream (mode (eql :multiline)) (instruction fill-newline))
  (declare (ignore client))
  (if (and (or (not (break-before-p instruction))
               (not (section-end instruction)))
           (not (miser-p stream instruction)))
      :maybe-break
      (call-next-method)))

(defmethod layout
    (client stream (mode (eql :single-line)) (instruction newline))
  (declare (ignore client stream))
  :no-break)

(defmethod layout (client stream mode (instruction newline))
  (cond ((and (not *print-readably*)
              *print-lines*
              (>= (1+ (line instruction)) *print-lines*))
         (add-text-fragment client stream mode instruction "..")
         :overflow-lines)
        (t
         (add-newline-fragment client stream mode instruction)
         (when (parent instruction)
           (map nil (lambda (fragment)
                      (vector-push-extend fragment (fragments stream)))
                (prefix-fragments (parent instruction)))
           (unless (typep instruction 'literal-newline)
             (add-tab-fragment client stream mode instruction
                               (if *print-miser-width*
                                   (start-column (parent instruction))
                                   (+ (start-column (parent instruction))
                                      (indent (parent instruction)))))))
         :break)))

(defmethod layout (client stream mode (instruction block-indent))
  (declare (ignore client stream mode))
  (setf (indent (parent instruction))
        (width instruction))
  :no-break)

(defmethod layout (client stream mode (instruction current-indent))
  (declare (ignore client stream mode))
  (setf (indent (parent instruction))
        (+ (width instruction)
           (column instruction)
           (- (start-column (parent instruction)))))
  :no-break)

(defmethod layout (client stream (mode (eql :overflow-lines)) (instruction block-start))
  (declare (ignore client stream))
  (setf (suffix (block-end instruction)) "")
  :no-break)

(defmethod layout (client stream mode (instruction block-start))
  (let* ((column (column instruction))
         (start-column (+ column
                          (stream-measure-string (target stream)
                                                 (prefix instruction))))
         (parent-prefix-fragments (when (parent instruction)
                                    (prefix-fragments (parent instruction))))
         (prefix (prefix instruction))
         (per-line-prefix-p (per-line-prefix-p instruction)))
    (setf (start-column instruction) start-column
          (indent instruction) 0)
    (cond (parent-prefix-fragments
           (setf (prefix-fragments instruction)
                 (make-array (length parent-prefix-fragments)
                             :fill-pointer (length parent-prefix-fragments)
                             :adjustable t
                             :initial-contents parent-prefix-fragments))
           (when column
             (vector-push-extend column (prefix-fragments instruction)))
           (when per-line-prefix-p
             (vector-push-extend prefix
                                 (prefix-fragments instruction))))
          (per-line-prefix-p
           (setf (prefix-fragments instruction)
                 (vector column prefix))))
    (add-text-fragment client stream mode instruction
                       (prefix instruction))))

(defmethod layout (client stream (mode (eql :overflow-lines)) (instruction block-end))
  (add-text-fragment client stream mode instruction (suffix instruction)))

(defmethod layout (client stream mode (instruction block-end))
  (add-text-fragment client stream mode instruction (suffix instruction)))

(defun push-instruction (instruction stream &aux (current-tail (tail stream)))
  (if current-tail
      (setf (next current-tail) instruction
            (previous instruction) current-tail)
      (setf (head stream) instruction))
  (setf (tail stream) instruction))

(declaim (inline do-pprint-newline))

(defun do-pprint-newline (stream newline)
  (with-accessors ((sections sections))
      stream
    (let ((parent (car (blocks stream)))
          (depth (length (blocks stream))))
      (setf (style newline) (stream-style stream)
            (parent newline) parent
            (depth newline) depth
            sections (delete-if (lambda (s)
                                  (when (or (eq (parent s) parent)
                                            (eq s parent)
                                            (and (typep s 'newline)
                                                 (> (depth s) depth)))
                                    (setf (section-end s) newline)
                                    (loop for i = (next s) then (next i)
                                          while (and i (not (eq i newline)))
                                          when (typep i 'newline)
                                            do (pushnew newline (newlines-after i)))
                                    t))
                                sections)
            (section newline) (car (sections stream)))
      (push newline sections)
      (push-instruction newline stream))))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :fresh)))
  (declare (ignore client))
  (do-pprint-newline stream (make-instance 'fresh-newline)))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :fresh-literal)))
  (declare (ignore client))
  (do-pprint-newline stream (make-instance 'fresh-literal-newline)))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :mandatory)))
  (declare (ignore client))
  (do-pprint-newline stream (make-instance 'mandatory-newline)))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :mandatory-literal)))
  (declare (ignore client))
  (do-pprint-newline stream (make-instance 'mandatory-literal-newline)))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :miser)))
  (declare (ignore client))
  (do-pprint-newline stream (make-instance 'miser-newline)))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :miser-literal)))
  (declare (ignore client))
  (do-pprint-newline stream (make-instance 'miser-literal-newline)))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :linear)))
  (declare (ignore client))
  (do-pprint-newline stream (make-instance 'linear-newline)))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :linear-literal)))
  (declare (ignore client))
  (do-pprint-newline stream (make-instance 'linear-literal-newline)))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :fill)))
  (declare (ignore client))
  (do-pprint-newline stream (make-instance 'fill-newline)))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :fill-literal)))
  (declare (ignore client))
  (do-pprint-newline stream (make-instance 'fill-literal-newline)))

(defmethod pprint-tab (client (stream pretty-stream) (kind (eql :line)) colnum colinc)
  (declare (ignore client))
  (push-instruction (make-instance 'line-tab
                                   :colnum colnum :colinc colinc
                                   :style (stream-style stream)
                                   :section (car (sections stream))
                                   :parent (car (blocks stream)))
                    stream))

(defmethod pprint-tab (client (stream pretty-stream) (kind (eql :line-relative)) colnum colinc)
  (declare (ignore client))
  (push-instruction (make-instance 'line-relative-tab
                                   :colnum colnum :colinc colinc
                                   :style (stream-style stream)
                                   :section (car (sections stream))
                                   :parent (car (blocks stream)))
                    stream))

(defmethod pprint-tab (client (stream pretty-stream) (kind (eql :section)) colnum colinc)
  (declare (ignore client))
  (push-instruction (make-instance 'section-tab
                                   :colnum colnum :colinc colinc
                                   :style (stream-style stream)
                                   :section (car (sections stream))
                                   :parent (car (blocks stream)))
                    stream))

(defmethod pprint-tab (client (stream pretty-stream) (kind (eql :section-relative)) colnum colinc)
  (declare (ignore client))
  (push-instruction (make-instance 'section-relative-tab
                                   :colnum colnum :colinc colinc
                                   :style (stream-style stream)
                                   :section (car (sections stream))
                                   :parent (car (blocks stream)))
                    stream))

(defmethod pprint-indent (client (stream pretty-stream) (relative-to (eql :block)) n)
  (declare (ignore client))
  (push-instruction (make-instance 'block-indent
                                   :width n
                                   :style (stream-style stream)
                                   :section (car (sections stream))
                                   :parent (car (blocks stream)))
                    stream))

(defmethod pprint-indent (client (stream pretty-stream) (relative-to (eql :current)) n)
  (declare (ignore client))
  (push-instruction (make-instance 'current-indent
                                   :width n
                                   :style (stream-style stream)
                                   :section (car (sections stream))
                                   :parent (car (blocks stream)))
                    stream))

(defun get-text-buffer (stream &aux (current-tail (tail stream)))
  (value (if (typep current-tail 'text)
             current-tail
             (push-instruction (make-instance 'text
                                              :section (car (sections stream))
                                              :style (stream-style stream)
                                              :parent (car (blocks stream)))
                               stream))))

(defmethod make-pretty-stream (client (stream broadcast-stream))
  (declare (ignore client))
  (if (broadcast-stream-streams stream)
      (call-next-method)
      stream))

(defmethod make-pretty-stream (client (stream two-way-stream))
  (make-pretty-stream client (two-way-stream-output-stream stream)))

(defmethod make-pretty-stream (client (stream pretty-stream))
  (declare (ignore client))
  stream)

(defmethod make-pretty-stream (client stream)
  (make-instance 'pretty-stream :target stream :client client))

#+sbcl
(defmethod make-pretty-stream (client (stream sb-pretty:pretty-stream))
  (make-pretty-stream client (sb-pretty::pretty-stream-target stream)))

(defmethod pretty-stream-p (client (stream pretty-stream))
  (declare (ignore client))
  t)

(defmethod pprint-start-logical-block (client (stream pretty-stream) prefix per-line-prefix-p)
  (let ((block-start (make-instance 'block-start
                                    :section (car (sections stream))
                                    :style (stream-style stream)
                                    :prefix (normalize-text client stream prefix)
                                    :per-line-prefix-p per-line-prefix-p
                                    :depth (length (blocks stream))
                                    :parent (car (blocks stream)))))
    (push block-start (blocks stream))
    (push block-start (sections stream))
    (push-instruction block-start stream)))

(defmethod pprint-end-logical-block (client (stream pretty-stream) suffix)
  (let ((block-end (make-instance 'block-end
                                  :section (car (sections stream))
                                  :style (stream-style stream)
                                  :suffix (normalize-text client stream suffix)
                                  :parent (car (blocks stream)))))
    (setf (block-end (car (blocks stream))) block-end)
    (pop (blocks stream))
    (push-instruction block-end stream)
    (process-instructions stream)))

(defun frob-style (stream style &aux (current-tail (tail stream)))
  (cond (style)
        (current-tail
         (style current-tail))
        (t
         (stream-style stream))))

;;; Gray Stream protocol support

#+(or)(defmethod ngray:stream-file-position ((stream pretty-stream))
  (file-position (target stream)))

(defmethod ngray:stream-write-char ((stream pretty-stream) char)
  (cond ((null (blocks stream))
         (write-char char (target stream)))
        ((char= char #\Newline)
         (pprint-newline (client stream) stream :mandatory-literal))
        (t
         (vector-push-extend char (get-text-buffer stream))))
  char)

(defmethod ngray:stream-write-string ((stream pretty-stream) string &optional start end)
  (if (blocks stream)
      (flet ((append-text (start2 end2)
               (when (/= start2 end2)
                 (let* ((buffer (get-text-buffer stream))
                        (start1 (fill-pointer buffer))
                        (end1 (+ start1 (- end2 start2))))
                   (when (< (array-total-size buffer) end1)
                     (adjust-array buffer (+ 32 end1)))
                   (setf (fill-pointer buffer) end1)
                   (replace buffer string :start1 start1 :end1 end1 :start2 start2 :end2 end2)))))
        (prog (pos)
          (unless start
            (setf start 0))
          (unless end
            (setf end (length string)))
         next
          (setf pos (position #\newline string :start start :end end))z
          (when pos
            (append-text start pos)
            (pprint-newline (client stream) stream :mandatory-literal)
            (setf start (1+ pos))
            (go next))
          (append-text start end)))
      (write-string string (target stream) :start (or start 0) :end end))
  string)

(defmethod ngray:stream-finish-output ((stream pretty-stream))
  (unless (blocks stream)
    (finish-output (target stream))))

(defmethod ngray:stream-force-output ((stream pretty-stream))
  (unless (blocks stream)
    (force-output (target stream))))

(defmethod ngray:stream-clear-output ((stream pretty-stream))
  (unless (blocks stream)
    (clear-output (target stream))))

(defmethod ngray:stream-terpri ((stream pretty-stream))
  (if (blocks stream)
      (pprint-newline (client stream) stream :mandatory-literal)
      (terpri (target stream))))

(defmethod ngray:stream-fresh-line ((stream pretty-stream))
  (if (blocks stream)
      (pprint-newline (client stream) stream :fresh-literal)
      (fresh-line (target stream))))

(defmethod ngray:stream-line-column ((stream pretty-stream) &aux (current-tail (tail stream)))
  (or (and current-tail
           (column current-tail))
      (and (blocks stream)
           (start-column (car (blocks stream))))
      (ngray:stream-line-column (target stream))))

(defmethod ngray:stream-advance-to-column ((stream pretty-stream) column)
  (cond ((blocks stream)
         (push-instruction (make-instance 'advance
                                          :style (stream-style stream)
                                          :section (car (sections stream))
                                          :value column
                                          :parent (car (blocks stream)))
                           stream)
         t)
        ((ngray:stream-advance-to-column (target stream) column))))

(defmethod stream-style ((stream pretty-stream) &aux (current-tail (tail stream)))
  (if current-tail
      (style current-tail)
      (stream-style (target stream))))

(defmethod (setf stream-style) (new-style (stream pretty-stream))
  (when new-style
    (if (blocks stream)
        (push-instruction (make-instance 'style
                                         :style new-style
                                         :section (car (sections stream))
                                         :parent (car (blocks stream)))
                          stream)
        (setf (stream-style (target stream)) new-style))))

(defmethod make-style (client (stream pretty-stream) &rest initargs &key)
  (apply #'make-style client (target stream) initargs))

(defmethod stream-scale-column ((stream pretty-stream) column old-style new-style)
  (stream-scale-column (target stream) column
                       (frob-style stream old-style)
                       (frob-style stream new-style)))

(defmethod stream-measure-char ((stream pretty-stream) char &optional style)
  (stream-measure-char (target stream) char (frob-style stream style)))

(defmethod stream-measure-string ((stream pretty-stream) char &optional start end style)
  (stream-measure-string (target stream) char
                         (or start 0) end
                         (frob-style stream style)))
