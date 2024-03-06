(in-package #:inravina)

(declaim (inline line-length ancestor-p))

(defclass instruction ()
  ((parent :accessor parent
           :initarg :parent
           :type (or null block-start))
   (next :accessor next
         :initarg :next
         :initform nil
         :type (or null instruction))
   (section :accessor section
            :initarg :section
            :type (or null section-start))))

(defclass section-start (instruction)
  ((depth :accessor depth
          :initarg :depth
          :initform 0
          :type integer)
   (column :accessor column
           :initarg :column
           :initform nil
           :type (or null real))
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
  ((value :accessor value
          :initarg :value
          :initform nil)))

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
                   :type boolean)))

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
   (indent :accessor indent
           :initarg :indent
           :initform nil
           :type (or null real))
   (line-width :accessor line-width
               :initarg :line-width
               :initform nil
               :type (or null real))
   (miser-width :reader miser-width
                :initarg :miser-width
                :initform nil
                :type (or null real))
   (miser-style :accessor miser-style-p
                :initform nil
                :type boolean)
   (block-end :accessor block-end
              :initarg :block-end
              :initform nil    
              :type (or null block-end))))

(defmethod indent ((instance instruction))
  (indent (parent instance)))

(defmethod (setf indent) (new-value (instance instruction))
  (setf (indent (parent instance)) new-value))

(defmethod line-width (instruction)
  (line-width (parent instruction)))

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
   (depth :accessor depth
          :initform 0)
   (fragments :reader fragments
              :initform (make-array 32
                                    :adjustable t
                                    :fill-pointer 0
                                    :element-type '(or null style string real)))
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
          :initform nil)
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

#+pprint-debug
(defparameter *debug-instruction* nil)

#+pprint-debug
(defparameter *debug-section* nil)

#+pprint-debug
(defparameter *pprint-debug* nil)

#+pprint-debug
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
  (loop for i = (head stream) then (next i)
        while i
        when (and (typep (section i) 'block-start)
                  (null (section-end (section i))))
          do (setf (section i) (section (section i))))
  (prog ((section t)
         last-maybe-break
         status
         (mode :single-line)
         (client (client stream))
         (instruction (head stream))
         (%fragments-length 0)
         (%indent 0)
         (%column (or (ngray:stream-line-column (target stream)) 0))
         (%style (stream-style (target stream))))
     (setf (line stream) 0
           (column stream) %column
           (style stream) %style)
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
             ((or (eq status :no-break)
                  (eq status :maybe-break))
              (cond ((and (or (null section)
                              (and (typep section 'section-start)
                                   (eq instruction (section-end section))))
                          (or (typep instruction 'newline)
                              (and (typep instruction 'block-start)
                                   (section-end instruction))))
                     (setf section instruction
                           %fragments-length (length (fragments stream))
                           %indent (indent instruction)
                           %column (column stream)
                           %style (style stream)))
                    ((or (eq section instruction)
                         (and (typep section 'section-start)
                              (eq instruction (section-end section))))
                     (setf section nil)))
              (when (and (eq status :maybe-break)
                         (or (null last-maybe-break)
                             (ancestor-p last-maybe-break (parent instruction))))
                (setf last-maybe-break instruction))
              (setf instruction (next instruction)
                    mode (if (or (null section)
                                 (and (typep section 'section-start)
                                      (or (eq section instruction)
                                          (eq (section-end section) instruction))))
                             :multiline
                             :single-line)))
             ((eq status :break)
              (when (and (section instruction)
                         (section-end (section instruction)))
                (setf (break-before-p (section-end (section instruction))) t))
              (setf section (and (not (eq section instruction))
                                 instruction)
                    mode (if section :single-line :multiline)
                    last-maybe-break nil
                    instruction (next instruction))
              (when section
                (setf %fragments-length (length (fragments stream))
                      %indent (indent instruction)
                      %column (column stream)
                      %style (style stream))))
             ((eq status :overflow-lines)
              (setf mode :overflow-lines
                    section nil
                    instruction (next instruction)))
             (last-maybe-break
              (setf instruction last-maybe-break
                    (fill-pointer (fragments stream)) %fragments-length
                    (indent instruction) %indent
                    (column stream) %column
                    (style stream) %style
                    section last-maybe-break
                    last-maybe-break nil
                    mode :unconditional))
             (section
              (setf instruction (if (eq t section)
                                    (head stream)
                                    (next section))
                    section nil
                    mode :multiline
                    (fill-pointer (fragments stream)) %fragments-length
                    (indent instruction) %indent
                    (column stream) %column
                    (style stream) %style))
             (t
              (setf mode :unconditional)))
       (go repeat)))
  (setf (head stream) nil
        (tail stream) nil))
        
(defun write-fragments (stream)
  (loop with client = (client stream)
        with target = (target stream)
        with fragments = (fragments stream)
        for fragment across fragments
        for i from 0
        do (etypecase fragment
             (string
              (write-string fragment target))
             (cons
              (write-string (car fragment) target :end (cdr fragment)))
             (null)
             (real
              (unless (minusp fragment)
                (ngray:stream-advance-to-column target fragment)))
             (style
              (setf (stream-style stream) (value fragment))))
        finally (setf (fill-pointer fragments) 0)))

(defun process-instructions (stream)
  (unless (blocks stream)
    (layout-instructions stream)
    (write-fragments stream)
    (setf (sections stream) nil)
    (finish-output (target stream))))

(defgeneric layout (client stream mode instruction)
  (:method (client stream (mode (eql :overflow-lines)) instruction)
    (declare (ignore client stream mode instruction))
    :no-break))

(defmethod layout :before (client stream mode (instruction section-start))
  (declare (ignore client mode))
  (setf (column instruction) (column stream)))

(defun add-advance-fragment (stream mode instruction column)
  (declare (ignore mode instruction))
  (with-accessors ((fragments fragments))
      stream
    (if (and (plusp (length fragments))
             (typep (aref fragments (1- (length fragments))) 'real))
        (setf (aref fragments (1- (length fragments))) column)
        (vector-push-extend column fragments))
    (setf (column stream) column)
    :no-break))

(defun add-text-fragment (stream mode instruction text)
  (if (or (null text)
          (zerop (length text)))
      :no-break
      (let ((new-column (+ (column stream)
                           (stream-measure-string (target stream) text
                                                  (style stream)))))
        (when (or (member mode '(:unconditional :overflow-lines))
                  (>= (line-width instruction) new-column))
          (setf (column stream) new-column)
          (vector-push-extend text (fragments stream))
          :no-break))))

(defmethod layout (client stream mode (instruction advance))
  (add-advance-fragment stream mode instruction (value instruction)))

(defmethod layout (client stream mode (instruction text))
  (add-text-fragment stream mode instruction (value instruction)))

(defmethod layout (client stream mode (instruction style))
  (declare (ignore client mode))
  (with-accessors ((value value))
      instruction
    (setf (column stream) (stream-scale-column (target stream) (column stream)
                                               (style stream) value)
          (style stream) value)
    (vector-push-extend instruction (fragments stream))))

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
                   &aux (column (column stream)))
  (add-advance-fragment stream mode instruction
                        (+ column
                           (compute-tab-size (- column
                                                (column (or (section instruction)
                                                            (head stream))))
                                             (colnum instruction)
                                             (colinc instruction)
                                             (typep instruction 'relative-tab)))))

(defmethod layout (client stream mode (instruction line-tab)
                   &aux (column (column stream)))
  (add-advance-fragment stream mode instruction
                        (+ column
                           (compute-tab-size column
                                             (colnum instruction)
                                             (colinc instruction)
                                             (typep instruction 'relative-tab)))))

(defmethod layout
    (client stream (mode (eql :single-line)) (instruction mandatory-newline))
  (declare (ignore client stream))
  nil)

(defmethod layout
    (client stream (mode (eql :multiline)) (instruction miser-newline))
  (declare (ignore client))
  (if (miser-style-p (parent instruction))
      (call-next-method)
      :no-break))

(defmethod layout
    (client stream (mode (eql :multiline)) (instruction fresh-newline))
  (declare (ignore client))
  (if (zerop (column stream))
      :no-break
      (call-next-method)))

(defmethod layout (client stream (mode (eql :multiline)) (instruction fill-newline))
  (declare (ignore client))
  (if (and (not (break-before-p instruction))
           (not (miser-style-p (parent instruction))))
      :maybe-break
      (call-next-method)))

(defmethod layout
    (client stream (mode (eql :single-line)) (instruction newline))
  (declare (ignore client stream))
  :no-break)

(defmethod layout (client stream mode (instruction newline))
  (cond ((and (not *print-readably*)
              *print-lines*
              (>= (1+ (line stream)) *print-lines*))
         (add-text-fragment stream :overflow-lines instruction "..")
         :overflow-lines)
        (t
         (loop with fragments = (fragments stream)
               for index from (1- (length fragments)) downto 0
               for fragment = (aref fragments index)
               do (typecase fragment
                    (number
                     (setf (aref fragments index) nil))
                    (string
                     (let ((pos (break-position client stream fragment)))
                       (cond ((zerop pos)
                              (setf (aref fragments index) nil))
                             ((= pos (length fragment))
                              (loop-finish))
                             (t
                              (setf (aref fragments index)
                                    (cons fragment pos))
                              (loop-finish)))))))
         (write-fragments stream)
         (terpri (target stream))
         (setf (column stream) (column (parent instruction))
               (column instruction) (column (parent instruction)))
         (incf (line stream))
         (when (parent instruction)
           (map nil
                (lambda (fragment)
                  (vector-push-extend fragment (fragments stream)))
                (prefix-fragments (parent instruction)))
           (unless (typep instruction 'literal-newline)
             (add-advance-fragment stream mode instruction
                                   (if (miser-style-p (parent instruction))
                                       (column (parent instruction))
                                       (+ (column (parent instruction))
                                          (indent instruction))))))
         :break)))

(defmethod layout (client stream mode (instruction block-indent))
  (declare (ignore client stream mode))
  (setf (indent instruction) (width instruction))
  :no-break)

(defmethod layout (client stream mode (instruction current-indent))
  (declare (ignore client mode))
  (setf (indent instruction)
        (+ (width instruction)
           (column stream)
           (- (column (parent instruction)))))
  :no-break)

(defmethod layout (client stream (mode (eql :overflow-lines)) (instruction block-start))
  (declare (ignore client stream))
  (setf (suffix (block-end instruction)) "")
  :no-break)

(defmethod layout (client stream mode (instruction block-start))
  (with-accessors ((column column))
      stream
    (with-accessors ((block-column column)
                     (prefix prefix)
                     (indent indent)
                     (parent parent)
                     (line-width line-width)
                     (miser-width miser-width)
                     (miser-style-p miser-style-p)
                     (prefix-fragments prefix-fragments)
                     (per-line-prefix-p per-line-prefix-p))
        instruction
      (let ((result (add-text-fragment stream mode instruction prefix)))
        (when result
          (setf miser-style-p (and miser-width
                                   line-width
                                   column
                                   (<= (- line-width column)
                                       miser-width))
                indent 0)
          (setf prefix-fragments
                (append (and parent (prefix-fragments parent))
                        (when (and per-line-prefix-p
                                   (not (zerop (length prefix))))
                          (list prefix))))
          (setf block-column column))
        result))))

(defmethod layout (client stream (mode (eql :overflow-lines)) (instruction block-end))
  (add-text-fragment stream mode instruction (suffix instruction)))

(defmethod layout (client stream mode (instruction block-end))
  (add-text-fragment stream mode instruction (suffix instruction)))

(defun push-instruction (instruction stream &aux (current-tail (tail stream)))
  (if current-tail
      (setf (next current-tail) instruction)
      (setf (head stream) instruction))
  (setf (tail stream) instruction))

(defmacro do-pprint-newline (stream newline-class)
  `(with-accessors ((blocks blocks)
                    (depth depth)
                    (sections sections))
       ,stream
     (let* ((parent (car blocks))
            (newline (make-instance ',newline-class
                                    :parent parent
                                    :depth depth)))
       ;; Terminate open sections. The section stack is FILO. Therefore
       ;; the first sections will be unterminated newlines in the same
       ;; block. Next will be unterminated block-start. Finally will be
       ;; newlines at a larger depth.
       (prog ((head sections) section)
        repeat
          (when head
            (setf section (car head))
            (when (or (and (typep section 'block-start)
                           (eq section parent))
                      (and (typep section 'newline)
                           (or (eq (parent section) parent)
                               (> (depth section) depth))))
              (setf (section-end section) newline
                    sections (cdr head)))
            (setf head (cdr head))
            (go repeat)))
       (setf (section newline) (car sections))
       (push newline sections)
       (push-instruction newline ,stream))))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :fresh)))
  (declare (ignore client))
  (do-pprint-newline stream fresh-newline))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :fresh-literal)))
  (declare (ignore client))
  (do-pprint-newline stream fresh-literal-newline))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :mandatory)))
  (declare (ignore client))
  (do-pprint-newline stream mandatory-newline))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :mandatory-literal)))
  (declare (ignore client))
  (do-pprint-newline stream mandatory-literal-newline))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :miser)))
  (declare (ignore client))
  (when (miser-width (car (blocks stream)))
    (do-pprint-newline stream miser-newline)))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :miser-literal)))
  (declare (ignore client))
  (when (miser-width (car (blocks stream)))
    (do-pprint-newline stream miser-literal-newline)))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :linear)))
  (declare (ignore client))
  (do-pprint-newline stream linear-newline))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :linear-literal)))
  (declare (ignore client))
  (do-pprint-newline stream linear-literal-newline))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :fill)))
  (declare (ignore client))
  (do-pprint-newline stream fill-newline))

(defmethod pprint-newline (client (stream pretty-stream) (kind (eql :fill-literal)))
  (declare (ignore client))
  (do-pprint-newline stream fill-literal-newline))

(defmethod pprint-tab (client (stream pretty-stream) (kind (eql :line)) colnum colinc)
  (declare (ignore client))
  (push-instruction (make-instance 'line-tab
                                   :colnum colnum :colinc colinc
                                   :section (car (sections stream))
                                   :parent (car (blocks stream)))
                    stream))

(defmethod pprint-tab (client (stream pretty-stream) (kind (eql :line-relative)) colnum colinc)
  (declare (ignore client))
  (push-instruction (make-instance 'line-relative-tab
                                   :colnum colnum :colinc colinc
                                   :section (car (sections stream))
                                   :parent (car (blocks stream)))
                    stream))

(defmethod pprint-tab (client (stream pretty-stream) (kind (eql :section)) colnum colinc)
  (declare (ignore client))
  (push-instruction (make-instance 'section-tab
                                   :colnum colnum :colinc colinc
                                   :section (car (sections stream))
                                   :parent (car (blocks stream)))
                    stream))

(defmethod pprint-tab (client (stream pretty-stream) (kind (eql :section-relative)) colnum colinc)
  (declare (ignore client))
  (push-instruction (make-instance 'section-relative-tab
                                   :colnum colnum :colinc colinc
                                   :section (car (sections stream))
                                   :parent (car (blocks stream)))
                    stream))

(defmethod pprint-indent (client (stream pretty-stream) (relative-to (eql :block)) n)
  (declare (ignore client))
  (push-instruction (make-instance 'block-indent
                                   :width n
                                   :section (car (sections stream))
                                   :parent (car (blocks stream)))
                    stream))

(defmethod pprint-indent (client (stream pretty-stream) (relative-to (eql :current)) n)
  (declare (ignore client))
  (push-instruction (make-instance 'current-indent
                                   :width n
                                   :section (car (sections stream))
                                   :parent (car (blocks stream)))
                    stream))

(defun get-text-buffer (stream &aux (current-tail (tail stream)))
  (value (if (typep current-tail 'text)
             current-tail
             (push-instruction (make-instance 'text
                                              :section (car (sections stream))
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
                                    :prefix (normalize-text client stream prefix)
                                    :per-line-prefix-p per-line-prefix-p
                                    :miser-width *print-miser-width*
                                    :line-width (line-length stream)
                                    :depth (depth stream)
                                    :parent (car (blocks stream)))))
    (incf (depth stream))
    (push block-start (blocks stream))
    (push block-start (sections stream))
    (push-instruction block-start stream)))

(defmethod pprint-end-logical-block (client (stream pretty-stream) suffix)
  (let ((block-end (make-instance 'block-end
                                  :section (car (sections stream))
                                  :suffix (normalize-text client stream suffix)
                                  :parent (car (blocks stream)))))
    (setf (block-end (car (blocks stream))) block-end)
    (pop (blocks stream))
    (decf (depth stream))
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
  (if (blocks stream)
      (vector-push-extend char (get-text-buffer stream))
      (write-char char (target stream)))
  char)

(defmethod ngray:stream-write-char ((stream pretty-stream) (char (eql #\Newline)))
  (if (blocks stream)
      (do-pprint-newline stream mandatory-literal-newline)
      (terpri (target stream)))
  char)

#|
(defmethod ngray:stream-write-string
    ((stream pretty-stream) string &optional start end)
  (if (blocks stream)
      (flet ((append-text (start2 end2)
               (when (/= start2 end2)
                 (let* ((buffer (get-text-buffer stream))
                        (start1 (fill-pointer buffer))
                        (end1 (+ start1 (- end2 start2))))
                   (when (< (array-total-size buffer) end1)
                     (adjust-array buffer (+ 32 end1)))
                   (setf (fill-pointer buffer) end1)
                   (replace buffer string
                            :start1 start1 :end1 end1
                            :start2 start2 :end2 end2)))))
        (prog (pos)
          (unless start
            (setf start 0))
          (unless end
            (setf end (length string)))
         next
           (setf pos (position #\newline string
                               :start start :end end))
          (when pos
            (append-text start pos)
            (do-pprint-newline stream mandatory-literal-newline)
            (setf start (1+ pos))
            (go next))
          (append-text start end)))
      (write-string string (target stream)
                    :start (or start 0) :end end))
  string)

#+gray-streams-sequence
(defmethod ngray:stream-write-sequence
    #+gray-streams-sequence/optional
    ((stream pretty-stream) sequence &optional start end)
    #+gray-streams-sequence/required
    ((stream pretty-stream) sequence start end)
    #+gray-streams-sequence/key
    (sequence (stream pretty-stream) &key start end)
  (ngray:stream-write-string stream sequence start end))
|#

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
      (do-pprint-newline stream mandatory-literal-newline)
      (terpri (target stream)))
  nil)

(defmethod ngray:stream-fresh-line ((stream pretty-stream))
  (if (blocks stream)
      (do-pprint-newline stream fresh-literal-newline)
      (fresh-line (target stream)))
  nil)

(defmethod ngray:stream-line-column ((stream pretty-stream))
  (if (blocks stream)
      (column stream)
      (ngray:stream-line-column (target stream))))

(defmethod ngray:stream-advance-to-column ((stream pretty-stream) column)
  (cond ((blocks stream)
         (push-instruction (make-instance 'advance
                                          :section (car (sections stream))
                                          :value column
                                          :parent (car (blocks stream)))
                           stream)
         t)
        ((ngray:stream-advance-to-column (target stream) column))))

#+gray-streams-line-length
(defmethod ngray:stream-line-length ((stream pretty-stream))
  (if (blocks stream)
      (line-width (car (blocks stream)))
      (ngray:stream-line-length (target stream))))

(defmethod ngray:stream-element-type ((stream pretty-stream))
  (ngray:stream-element-type (target stream)))

#+gray-streams-element-type/setf
(defmethod (setf ngray:stream-element-type) (new-value (stream pretty-stream))
  (setf (ngray:stream-element-type (target stream)) new-value))

#+gray-streams-external-format
(defmethod ngray:stream-external-format ((stream pretty-stream))
  (ngray:stream-external-format (target stream)))

#+gray-streams-external-format/setf
(defmethod (setf ngray:stream-external-format) (new-value (stream pretty-stream))
  (setf (ngray:stream-external-format (target stream)) new-value))

(defmethod stream-style ((stream pretty-stream))
  (if (blocks stream)
      (style stream)
      (stream-style (target stream))))

(defmethod (setf stream-style) (new-style (stream pretty-stream))
  (when new-style
    (if (blocks stream)
        (push-instruction (make-instance 'style
                                         :value new-style
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
