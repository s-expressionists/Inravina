;(require :sb-profile)
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

(defun fu (count)
  (loop repeat count
        do (with-output-to-string (stream)
             (incless-extrinsic:pprint a stream))))

(fu 100)

(sb-profile:profile "INRAVINA" "INRAVINA-EXTRINSIC" "INCLESS" "INCLESS-EXTRINSIC")

(fu 100)

(in-package #:sb-profile)

(sb-ext:without-package-locks
(defun sb-profile::print-profile-table (time-info-list)
  (let ((total-seconds 0.0)
        (total-consed 0)
        (total-calls 0)
        (total-gc-run-time 0)
        (seconds-width (length "seconds"))
        (consed-width (length "consed"))
        (calls-width (length "calls"))
        (sec/call-width 10)
        (gc-run-time-width (length "gc"))
        (name-width 6))
    (dolist (time-info time-info-list)
      (incf total-seconds (time-info-seconds time-info))
      (incf total-consed (time-info-consing time-info))
      (incf total-calls (time-info-calls time-info))
      (incf total-gc-run-time (time-info-gc-run-time time-info)))
    (setf seconds-width (max (length (format nil "~10,3F" total-seconds))
                             seconds-width)
          calls-width (max (length (format nil "~:D" total-calls))
                           calls-width)
          consed-width (max (length (format nil "~:D" total-consed))
                            consed-width)
          gc-run-time-width (max (length (format nil "~10,3F" (/ total-gc-run-time internal-time-units-per-second)))
                            gc-run-time-width))

    (flet ((dashes ()
             (dotimes (i (+ seconds-width consed-width calls-width
                            sec/call-width name-width
                            (* 5 3)))
               (write-char #\- *trace-output*))
             (terpri *trace-output*)))
      (format *trace-output* "~&~@{ ~v:@<~A~>~^|~}~%"
              seconds-width "seconds"
              (1+ gc-run-time-width) "gc"
              (1+ consed-width) "consed"
              (1+ calls-width) "calls"
              (1+ sec/call-width) "sec/call"
              (1+ name-width) "name")

      (dashes)

      (dolist (time-info time-info-list)
        (format *trace-output* "~v,3F | ~v,3F | ~v:D | ~v:D | ~10G | ~S~%"
                seconds-width (time-info-seconds time-info)
                gc-run-time-width (/ (time-info-gc-run-time time-info) internal-time-units-per-second)
                consed-width (time-info-consing time-info)
                calls-width (time-info-calls time-info)
                (/ (time-info-seconds time-info)
                   (float (time-info-calls time-info)))
                (time-info-name time-info)))

      (dashes)

      (format *trace-output* "~v,3F | ~v,3F | ~v:D | ~v:D |            | Total~%"
                seconds-width total-seconds
                gc-run-time-width (/ total-gc-run-time internal-time-units-per-second)
                consed-width total-consed
                calls-width total-calls)

      (format *trace-output*
              "~%estimated total profiling overhead: ~4,2F seconds~%"
              (* (overhead-total *overhead*) (float total-calls)))
      (format *trace-output*
              "~&overhead estimation parameters:~%  ~Ss/call, ~Ss total profiling, ~Ss internal profiling~%"
              (overhead-call *overhead*)
              (overhead-total *overhead*)
              (overhead-internal *overhead*))))))


(sb-profile:report :print-no-call-list nil)
