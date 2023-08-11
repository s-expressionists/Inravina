(ql:quickload :flamegraph)
(ql:quickload :inravina-extrinsic)

(defvar a '(defun layout-instructions (stream)
  #+pprint-debug (when *pprint-debug*
                   (describe stream *debug-io*))
  (prog ((section t)
         last-maybe-break
         status
         mode
         (client (client stream))
         (instruction (head stream)))
   repeat
     (when instruction
       #+pprint-debug (when *pprint-debug*
                        (let ((*debug-instruction* instruction)
                              (*debug-section* section))
                          (describe stream *debug-io*)
                          (finish-output *debug-io*)
                          (format *debug-io* "section ~a, instruction ~a, mode = ~a, allow-break-p = ~a~%"
                                  section
                                  instruction mode
                                  (or (not section)
                                      (and (typep section 'section-start)
                                           (or (eq section instruction)
                                               (eq (section-end section) instruction)))))))
       (setf status (layout client stream mode instruction
                            (or (not section)
                                (and (typep section 'section-start)
                                     (or (eq section instruction)
                                         (eq (section-end section) instruction)))))
             mode (and (eq :overflow mode) mode))
       #+pprint-debug
       (when *pprint-debug*
         (format *debug-io* "status = ~a, mode = ~a~%"
                 status mode))
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
   (tail stream) nil)))

(defun fu ()
  (loop repeat 1000000
        do (with-output-to-string (stream)
             (incless-extrinsic:pprint a stream))))

(fu)

(flamegraph:save-flame-graph ((first (uiop:command-line-arguments)))
  (fu))
