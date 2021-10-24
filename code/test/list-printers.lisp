(in-package #:inravina/test)

(define-test pprint-fill
  (let ((form '(abc defg hijkl)))
    (is equal
        "ABC DEFG HIJKL"
        (with-env (stream :right-margin 80)
          (inravina:pprint-fill inravina:*client* stream form nil nil)))
    (is equal
        "ABC DEFG
HIJKL"
        (with-env (stream :right-margin 10)
          (inravina:pprint-fill inravina:*client* stream form nil nil)))
    (is equal
        "(ABC DEFG HIJKL)"
        (with-env (stream :right-margin 80)
          (inravina:pprint-fill inravina:*client* stream form t nil)))
    (is equal
        "(ABC DEFG
 HIJKL)"
        (with-env (stream :right-margin 10)
          (inravina:pprint-fill inravina:*client* stream form t nil)))))

(define-test pprint-linear
  (let ((form '(abc defg hijkl)))
    (is equal
      "ABC DEFG HIJKL"
        (with-env (stream :right-margin 80)
          (inravina:pprint-linear inravina:*client* stream form nil nil)))
    (is equal
      "ABC
DEFG
HIJKL"
        (with-env (stream :right-margin 10)
          (inravina:pprint-linear inravina:*client* stream form nil nil)))
    (is equal
      "(ABC DEFG HIJKL)"
        (with-env (stream :right-margin 80)
          (inravina:pprint-linear inravina:*client* stream form t nil)))
    (is equal
      "(ABC
 DEFG
 HIJKL)"
        (with-env (stream :right-margin 10)
          (inravina:pprint-linear inravina:*client* stream form t nil)))))

(define-test pprint-tabular
  (let ((form '(abc defg hijkl)))
    (is equal
      "ABC   DEFG  HIJKL"
        (with-env (stream :right-margin 80)
          (inravina:pprint-tabular inravina:*client* stream form nil nil 6)))
    (is equal
      "ABC   DEFG
HIJKL"
        (with-env (stream :right-margin 10)
          (inravina:pprint-tabular inravina:*client* stream form nil nil 6)))
    (is equal
      "(ABC   DEFG  HIJKL)"
        (with-env (stream :right-margin 80)
          (inravina:pprint-tabular inravina:*client* stream form t nil 6)))
    (is equal
      "(ABC   DEFG
 HIJKL)"
        (with-env (stream :right-margin 11)
          (inravina:pprint-tabular inravina:*client* stream form t nil 6)))))


