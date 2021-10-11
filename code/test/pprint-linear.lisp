(in-package #:inravina/test)

(defun pprint-linear-test (right-margin object colon-p at-sign-p)
  (let ((*print-right-margin* right-margin))
    (with-output-to-string (s)
      (inravina:pprint-linear inravina:*client* s object colon-p at-sign-p))))

(define-test pprint-linear.1
  (is equal
      "ABC DEFG HIJKL"
      (pprint-linear-test 80 '(abc defg hijkl) nil nil)))

(define-test pprint-linear.2
  (is equal
      "ABC
DEFG
HIJKL"
      (pprint-linear-test 10 '(abc defg hijkl) nil nil)))

(define-test pprint-linear.3
  (is equal
      "(ABC DEFG HIJKL)"
      (pprint-linear-test 80 '(abc defg hijkl) t nil)))

(define-test pprint-linear.4
  (is equal
      "(ABC
 DEFG
 HIJKL)"
      (pprint-linear-test 10 '(abc defg hijkl) t nil)))

