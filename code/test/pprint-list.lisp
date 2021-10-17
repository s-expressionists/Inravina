(in-package #:inravina/test)

(defmacro with-env ((stream &key right-margin) &body body)
  `(let ((*print-right-margin* ,right-margin))
     (with-output-to-string (,stream)
       ,@body)))

(define-test pprint-fill.1
  (is equal
      "ABC DEFG HIJKL"
      (with-env (stream :right-margin 80)
        (inravina:pprint-fill inravina:*client* stream '(abc defg hijkl) nil nil))))

(define-test pprint-fill.2
  (is equal
      "ABC DEFG
HIJKL"
      (with-env (stream :right-margin 10)
        (inravina:pprint-fill inravina:*client* stream '(abc defg hijkl) nil nil))))

(define-test pprint-fill.3
  (is equal
      "(ABC DEFG HIJKL)"
      (with-env (stream :right-margin 80)
        (inravina:pprint-fill inravina:*client* stream '(abc defg hijkl) t nil))))

(define-test pprint-fill.4
  (is equal
      "(ABC DEFG
 HIJKL)"
      (with-env (stream :right-margin 10)
        (inravina:pprint-fill inravina:*client* stream '(abc defg hijkl) t nil))))

(define-test pprint-linear.1
  (is equal
      "ABC DEFG HIJKL"
      (with-env (stream :right-margin 80)
        (inravina:pprint-linear inravina:*client* stream '(abc defg hijkl) nil nil))))

(define-test pprint-linear.2
  (is equal
      "ABC
DEFG
HIJKL"
      (with-env (stream :right-margin 10)
        (inravina:pprint-linear inravina:*client* stream '(abc defg hijkl) nil nil))))

(define-test pprint-linear.3
  (is equal
      "(ABC DEFG HIJKL)"
      (with-env (stream :right-margin 80)
        (inravina:pprint-linear inravina:*client* stream '(abc defg hijkl) t nil))))

(define-test pprint-linear.4
  (is equal
      "(ABC
 DEFG
 HIJKL)"
      (with-env (stream :right-margin 10)
        (inravina:pprint-linear inravina:*client* stream '(abc defg hijkl) t nil))))

(define-test pprint-tagbody.1
  (is equal
      "(TAGBODY
 FU
  (WIBBLE 1)
 BAR
  (QUUX 2))"
      (with-env (stream :right-margin 12)
        (inravina:pprint-tagbody inravina:*client* stream
                                 '(tagbody fu (wibble 1) bar (quux 2)) t nil))))

