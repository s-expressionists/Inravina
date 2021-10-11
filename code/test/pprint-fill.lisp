(in-package #:inravina/test)

(defun pprint-fill-test (right-margin object colon-p at-sign-p)
  (let ((*print-right-margin* right-margin))
    (with-output-to-string (s)
      (inravina:pprint-fill inravina:*client* s object colon-p at-sign-p))))

(define-test pprint-fill.1
  (is equal
      "ABC DEFG HIJKL"
      (pprint-fill-test 80 '(abc defg hijkl) nil nil)))

(define-test pprint-fill.2
  (is equal
      "ABC DEFG
HIJKL"
      (pprint-fill-test 10 '(abc defg hijkl) nil nil)))

(define-test pprint-fill.3
  (is equal
      "(ABC DEFG HIJKL)"
      (pprint-fill-test 80 '(abc defg hijkl) t nil)))

(define-test pprint-fill.4
  (is equal
      "(ABC DEFG
 HIJKL)"
      (pprint-fill-test 10 '(abc defg hijkl) t nil)))

