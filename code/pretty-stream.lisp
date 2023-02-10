(in-package #:inravina)

(defclass instruction ()
  ((parent :reader parent
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
  ((depth :reader depth
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
  ((kind :reader kind
         :initarg :kind
         :type (member :block :current))
   (width :reader width
          :initarg :width    
          :type real)))

(defmethod print-object ((obj indent) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (prin1 (kind obj) stream)
    (write-char #\Space stream)
    (prin1 (width obj) stream)))

(defclass newline (section-start)
  ((kind :reader kind
         :initarg :kind
         :type newline-kind)))

(defmethod print-object ((obj newline) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (prin1 (kind obj) stream)))

(defclass tab (instruction)
  ((kind :reader kind
         :initarg :kind
         :type tab-kind)
   (colnum :reader colnum
           :initarg :colnum    
           :type (or null real))
   (colinc :reader colinc
           :initarg :colinc    
           :type (or null real))))

(defmethod print-object ((obj tab) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (prin1 (kind obj) stream)
    (write-char #\Space stream)
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
    (trivial-gray-streams:fundamental-character-output-stream)
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

(defmethod describe-object ((object pretty-stream) stream)
  (when *debug-instruction*
    (loop for instruction = (head stream) then (next instruction)
          for char = (if (eq instruction *debug-instruction*)
                             #\*
                             #\Space)
          finally (terpri stream)
          while instruction
          if (typep instruction 'text)
            do (dotimes (i (length (value instruction))) (write-char char stream))
          else
            do (write-char char stream)))
  (loop for instruction = (head stream) then (next instruction)
        while instruction
        do (typecase instruction
             (block-start (write-char #\< stream))
             (block-end (write-char #\> stream))
             (newline (write-char (case (kind instruction)
                                    (:fill #\f)
                                    (:linear #\l)
                                    (:mandatory #\x)
                                    (:miser #\m)
                                    (:fresh #\r)
                                    (:literal-fill #\F)
                                    (:literal-linear #\L)
                                    (:literal-mandatory #\X)
                                    (:literal-miser #\M)
                                    (:literal-fresh #\R))
                                  stream))
             (text (dotimes (i (length (value instruction)))
                     (write-char #\- stream)))
             (otherwise (write-char #\? stream))))
  (terpri stream)
  (loop for instruction = (head stream) then (next instruction)
        while instruction
        when (typep instruction 'section-start)
          do (loop with ch = #\Space
                   for sub = (head stream) then (next instruction)
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
      (trivial-stream-column:line-length (target stream))
      100))

(defun ancestor-p (instruction ancestor)
  (loop for parent = (parent instruction) then (parent parent)
        if (eq parent ancestor)
          return t
        else if (null parent)
          return nil))

(defun layout-instructions (stream)
  #+pprint-debug (describe stream *debug-io*)
  (prog ((section t)
         last-maybe-break
         status
         mode
         (client (client stream))
         (instruction (head stream)))
   repeat
     (when instruction
       #+pprint-debug (let ((*debug-instruction* instruction))
                        (describe stream *debug-io*)
                        (finish-output *debug-io*)
                        (format *debug-io* "section ~a, instruction ~a, mode = ~a, allow-break-p = ~a~%"
                                section
                                instruction mode
                                (or (not section)
                                    (and (typep section 'section-start)
                                         (or (eq section instruction)
                                             (eq (section-end section) instruction))))))
       (setf status (layout client stream mode instruction
                            (or (not section)
                                (and (typep section 'section-start)
                                     (or (eq section instruction)
                                         (eq (section-end section) instruction)))))
             mode (and (eq :overflow mode) mode))
       #+pprint-debug
       (format *debug-io* "status = ~a, mode = ~a~%"
               status mode)
       (case status
         ((t :maybe-break)
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
          (setf instruction (next instruction)))
         (:break
          (setf section (and (not (eq section instruction))
                             instruction)
                last-maybe-break nil
                instruction (next instruction)))
         (:overflow
          (setf mode :overflow
                instruction (next instruction)))
         (otherwise
          (cond (last-maybe-break
                 (setf instruction last-maybe-break
                       (fill-pointer (fragments stream)) (fragment-index last-maybe-break)
                       section last-maybe-break
                       last-maybe-break nil
                       mode t))
                (section
                 (setf instruction (if (eq t section)
                                       (head stream)
                                       (next section))
                       section nil
                       (fill-pointer (fragments stream)) (fragment-index instruction)))
                (t
                 (setf mode t)))))
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
              (when (text-before-newline-p stream i)
                (trivial-stream-column:advance-to-column fragment target)))
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

(defgeneric layout (client stream mode instruction allow-break-p)
  (:method (client stream (mode (eql :overflow)) instruction allow-break-p)
    (declare (ignore client stream mode instruction allow-break-p))
    t))

(defmethod layout :before (client stream mode instruction allow-break-p
                           &aux (previous (previous instruction)))
  (declare (ignore client allow-break-p mode))
  (if previous
      (setf (column instruction) (trivial-stream-column:scale-column (column previous)
                                                                     (target stream)
                                                                     :old-style (style previous)
                                                                     :new-style (style instruction))
            (line instruction) (line previous))
      (setf (column instruction) (trivial-stream-column:scale-column (or (trivial-stream-column:line-column (target stream)) 0)
                                                                     (target stream)
                                                                     :new-style (style instruction))
            (line instruction) 0))
  (setf (fragment-index instruction) (length (fragments stream))))

(defun add-newline-fragment (client stream mode instruction)
  (declare (ignore client mode))
  (vector-push-extend #'terpri (fragments stream))
  (setf (column instruction) 0)
  (incf (line instruction))
  t)

(defun add-tab-fragment (client stream mode instruction column)
  (declare (ignore client mode))
  (vector-push-extend column (fragments stream))
  (setf (column instruction) column)
  t)

(defun add-style-fragment (client stream mode instruction style)
  (declare (ignore client mode instruction))
  (vector-push-extend (lambda (stream)
                        (trivial-stream-column:set-style style stream))
                      (fragments stream))
  t)

(defun add-text-fragment (client stream mode instruction text)
  (declare (ignore client))
  (or (null text)
      (zerop (length text))
      (let ((new-column (+ (column instruction)
                           (trivial-stream-column:measure-string text (target stream)
                                                                 :style (style instruction)))))
        (when (or mode
                  (>= (line-length stream) new-column))
          (setf (column instruction) new-column)
          (vector-push-extend text (fragments stream))
          t))))

(defmethod layout (client stream mode (instruction advance) allow-break-p)
  (declare (ignore allow-break-p))
  (add-tab-fragment client stream mode instruction (value instruction)))

(defmethod layout (client stream mode (instruction text) allow-break-p)
  (declare (ignore allow-break-p))
  (add-text-fragment client stream mode instruction (value instruction)))

(defmethod layout (client stream mode (instruction style) allow-break-p)
  (declare (ignore allow-break-p))
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

(defmethod layout (client stream mode (instruction tab) allow-break-p)
  (declare (ignore allow-break-p))
  (let* ((section-column (if (and (section instruction)
                                  (section-kind-p (kind instruction)))
                           (column (section instruction))
                           0))
         (column (- (column instruction) section-column)))
    (add-tab-fragment client stream mode instruction
                      (+ section-column
                         column
                         (compute-tab-size column
                                           (colnum instruction)
                                           (colinc instruction)
                                           (relative-kind-p (kind instruction)))))))

(defun miser-p (stream instruction
                &aux (line-length (line-length stream))
                     (line-column (column (parent instruction))))
  (and *print-miser-width*
       (and line-length
            line-column
            (<= (- line-length line-column)
                *print-miser-width*))))

(defmethod layout (client stream mode (instruction newline) allow-break-p
                   &aux (miser-p (miser-p stream instruction)))
  (declare (ignore))
  (cond ((or (and (not allow-break-p)
                  (mandatory-kind-p (kind instruction)))
             (and (not mode)
                  allow-break-p
                  (miser-kind-p (kind instruction))
                  miser-p))
         nil)
        ((and (not mode)
              (or (not allow-break-p)
                  (and (fresh-kind-p (kind instruction))
                       (zerop (column instruction)))
                  (and (miser-kind-p (kind instruction))
                       (not miser-p))))
         t)
        ((and (not mode)
              (or (and (fill-kind-p (kind instruction))
                       (not miser-p))
                  #+(or)(and (miser-kind-p (kind instruction))
                       miser-p)))
         :maybe-break)
        ((and (not *print-readably*)
              *print-lines*
              (>= (1+ (line instruction)) *print-lines*))
         (add-text-fragment client stream mode instruction "..")
         :overflow)
        (t
         (add-newline-fragment client stream mode instruction)
         (when (parent instruction)
           (map nil (lambda (fragment)
                      (vector-push-extend fragment (fragments stream)))
                (prefix-fragments (parent instruction)))
           (unless (literal-kind-p (kind instruction))
             (add-tab-fragment client stream mode instruction
                               (if *print-miser-width*
                                   (start-column (parent instruction))
                                   (+ (start-column (parent instruction))
                                      (indent (parent instruction)))))))
         :break)))

(defmethod layout (client stream mode (instruction indent) allow-break-p)
  (declare (ignore client stream allow-break-p mode))
  (setf (indent (parent instruction))
        (ecase (kind instruction)
          (:block
           (width instruction))
          (:current
           (+ (width instruction)
              (column instruction)
              (- (start-column (parent instruction)))))))
  t)

(defmethod layout (client stream (mode (eql :overflow)) (instruction block-start) allow-break-p)
  (declare (ignore client stream allow-break-p))
  (setf (suffix (block-end instruction)) "")
  t)

(defmethod layout (client stream mode (instruction block-start) allow-break-p)
  (declare (ignore allow-break-p))
  (let* ((column (column instruction))
         (start-column (+ column
                          (trivial-stream-column:measure-string (prefix instruction)
                                                                (target stream))))
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

(defmethod layout (client stream (mode (eql :overflow)) (instruction block-end) allow-break-p)
  (declare (ignore allow-break-p))
  (add-text-fragment client stream mode instruction (suffix instruction)))

(defmethod layout (client stream mode (instruction block-end) allow-break-p)
  (declare (ignore allow-break-p))
  (add-text-fragment client stream mode instruction (suffix instruction)))

(defun push-instruction (instruction stream &aux (current-tail (tail stream)))
  (if current-tail
      (setf (next current-tail) instruction
            (previous instruction) current-tail)
      (setf (head stream) instruction))
  (setf (tail stream) instruction))

(defmethod pprint-newline (client (stream pretty-stream) kind)
  (declare (ignore client))
  (with-accessors ((sections sections))
                  stream
    (let* ((parent (car (blocks stream)))
           (depth (length (blocks stream)))
           (newline (make-instance 'newline
                                   :kind kind :depth depth
                                   :style (trivial-stream-column:stream-style stream)
                                   :parent parent)))
      (setf sections
            (delete-if (lambda (s)
                         (when (or (eq (parent s) parent)
                                   (eq s parent)
                                   (and (typep s 'newline)
                                        (> (depth s) depth)))
                           (setf (section-end s) newline)
                           t))
                       sections)
            (section newline) (car (sections stream)))
      (push newline sections)
      (push-instruction newline stream))))

(defmethod pprint-tab (client (stream pretty-stream) kind colnum colinc)
  (declare (ignore client))
  (push-instruction (make-instance 'tab
                                     :kind kind :colnum colnum :colinc colinc
                                     :style (trivial-stream-column:stream-style stream)
                                     :section (car (sections stream))
                                     :parent (car (blocks stream)))
                    stream))

(defmethod pprint-indent (client (stream pretty-stream) relative-to n)
  (declare (ignore client))
  (push-instruction (make-instance 'indent
                                     :kind relative-to :width n
                                     :style (trivial-stream-column:stream-style stream)
                                     :section (car (sections stream))
                                     :parent (car (blocks stream)))
                    stream))

(defun get-text-buffer (stream &aux (current-tail (tail stream)))
  (value (if (typep current-tail 'text)
             current-tail
             (push-instruction (make-instance 'text
                                       :section (car (sections stream))
                                       :style (trivial-stream-column:stream-style stream)
                                       :parent (car (blocks stream)))
                               stream))))

(defmethod make-pretty-stream (client (stream broadcast-stream))
  (declare (ignore client))
  (if (broadcast-stream-streams stream)
      (call-next-method)
      stream))

(defmethod make-pretty-stream (client (stream pretty-stream))
  (declare (ignore client))
  stream)

(defmethod make-pretty-stream (client (stream (eql nil)))
  (call-next-method client *standard-output*))

(defmethod make-pretty-stream (client (stream (eql t)))
  (call-next-method client *terminal-io*))

(defmethod make-pretty-stream (client stream)
  (make-instance 'pretty-stream :target stream :client client))

#+sbcl
(defmethod make-pretty-stream (client (stream sb-pretty:pretty-stream))
  (make-pretty-stream client (sb-pretty::pretty-stream-target stream)))

(defmethod pprint-start-logical-block (client (stream pretty-stream) prefix per-line-prefix-p)
  (let ((block-start (make-instance 'block-start
                                    :section (car (sections stream))
                                    :style (trivial-stream-column:stream-style stream)
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
                                  :style (trivial-stream-column:stream-style stream)
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
         (trivial-stream-column:style stream))))

;;; Gray Stream protocol support

(defmethod trivial-gray-streams:stream-file-position ((stream pretty-stream))
  (file-position (target stream)))

(defmethod trivial-gray-streams:stream-write-char ((stream pretty-stream) char)
  (cond ((null (blocks stream))
         (write-char char (target stream)))
        ((char= char #\Newline)
         (pprint-newline (client stream) stream :literal-mandatory))
        (t
         (vector-push-extend char (get-text-buffer stream))))
  char)

(defmethod trivial-gray-streams:stream-write-string ((stream pretty-stream) string &optional start end)
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
            (pprint-newline (client stream) stream :literal-mandatory)
            (setf start (1+ pos))
            (go next))
          (append-text start end)))
      (write-string string (target stream) :start (or start 0) :end end))
  string)

(defmethod trivial-gray-streams:stream-finish-output ((stream pretty-stream))
  (unless (blocks stream)
    (finish-output (target stream))))

(defmethod trivial-gray-streams:stream-force-output ((stream pretty-stream))
  (unless (blocks stream)
    (force-output (target stream))))

(defmethod trivial-gray-streams:stream-clear-output ((stream pretty-stream))
  (unless (blocks stream)
    (clear-output (target stream))))

(defmethod trivial-gray-streams:stream-terpri ((stream pretty-stream))
  (if (blocks stream)
      (pprint-newline (client stream) stream :literal-mandatory)
      (terpri (target stream))))

(defmethod trivial-gray-streams:stream-fresh-line ((stream pretty-stream))
  (if (blocks stream)
      (pprint-newline (client stream) stream :literal-fresh)
      (fresh-line (target stream))))

(defmethod trivial-gray-streams:stream-line-column ((stream pretty-stream) &aux (current-tail (tail stream)))
  (or (and current-tail
           (column current-tail))
      (and (blocks stream)
           (start-column (car (blocks stream))))
      (trivial-stream-column:line-column (target stream))))

(defmethod trivial-gray-streams:stream-advance-to-column ((stream pretty-stream) column)
  (cond ((blocks stream)
         (push-instruction (make-instance 'advance
                                          :value column
                                          :parent (car (blocks stream)))
                           stream)
         t)
        ((trivial-stream-column:advance-to-column column (target stream)))))

;;; trivial-stream-column protocal support

(defmethod trivial-stream-column:stream-style ((stream pretty-stream) &aux (current-tail (tail stream)))
  (if current-tail
      (style current-tail)
      (trivial-stream-column:style (target stream))))

(defmethod (setf trivial-stream-column:stream-style) (new-style (stream pretty-stream))
  (if (blocks stream)
      (push-instruction (make-instance 'style
                                       :style new-style
                                       :section (car (sections stream))
                                       :parent (car (blocks stream)))
                        stream)
      (trivial-stream-column:set-style new-style (target stream))))

(defmethod trivial-stream-column:stream-copy-style ((stream pretty-stream) style &rest overrides &key &allow-other-keys)
  (apply #'trivial-stream-column:copy-style
         (frob-style stream style) (target stream) overrides))

(defmethod trivial-stream-column:stream-scale-column ((stream pretty-stream) column old-style new-style)
  (trivial-stream-column:scale-column column (target stream)
                                      :old-style (frob-style stream old-style)
                                      :new-style (frob-style stream new-style)))

(defmethod trivial-stream-column:stream-measure-char ((stream pretty-stream) char &optional style)
  (trivial-stream-column:measure-char char stream :style (frob-style stream style)))

(defmethod trivial-stream-column:stream-measure-string ((stream pretty-stream) char &optional start end style)
  (trivial-stream-column:measure-string char stream
                                        :start (or start 0) :end end
                                        :style (frob-style stream style)))
