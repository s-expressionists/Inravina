(in-package #:inravina/test)

(define-test d-1
  (let ((table (inravina:copy-pprint-dispatch *client* :empty)))
    (is-values (inravina:pprint-dispatch *client* table 0)
      ((lambda (x y) (not (equal x y))) nil)
      (equal nil))))

(define-test d-2
  (let ((table (inravina:copy-pprint-dispatch *client* :empty)))
    (inravina:set-pprint-dispatch *client* table
                                  '(cons symbol)
                                  (lambda (s o)
                                    (declare (ignore s o))
                                    t)
                                  0)
    (multiple-value-bind (func pres)
                         (inravina:pprint-dispatch *client* table '(defun))
      (true pres)
      (true (funcall func nil nil)))))

