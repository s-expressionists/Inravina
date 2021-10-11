(in-package #:inravina/test)

(define-test d-1
  (let ((table (make-instance 'inravina::dispatch-table)))
    (is-values (inravina:pprint-dispatch inravina:*client* 0 table)
      (equal nil)
      (equal nil))))

(define-test d-2
  (let ((table (make-instance 'inravina::dispatch-table)))
    (inravina:set-pprint-dispatch inravina:*client*
                                  '(cons symbol)
                                  (lambda (s o)
                                    (declare (ignore s o))
                                    t)
                                  0
                                  table)
    (multiple-value-bind (func pres)
                         (inravina:pprint-dispatch inravina:*client* '(defun) table)
      (true pres)
      (true (funcall func nil nil)))))

