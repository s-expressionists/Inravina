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

(define-test pprint-bindings.1
  (is equal
      "((FU 1) (BAR 2))"
      (with-env (stream :right-margin 80)
        (inravina:pprint-bindings inravina:*client* stream
                                  '((fu 1) (bar 2)) t nil))))
(define-test pprint-bindings.2
  (is equal
      "((FU 1)
 (BAR 2))"
      (with-env (stream :right-margin 9)
        (inravina:pprint-bindings inravina:*client* stream
                                  '((fu 1) (bar 2)) t nil))))
(define-test pprint-bindings.3
  (is equal
      "((FU 1)
 (BAR
  2))"
      (with-env (stream :right-margin 8)
        (inravina:pprint-bindings inravina:*client* stream
                                  '((fu 1) (bar 2)) t nil))))
(define-test pprint-bindings.4
  (is equal
      "((FU
  1)
 (BAR
  2))"
      (with-env (stream :right-margin 6)
        (inravina:pprint-bindings inravina:*client* stream
                                  '((fu 1) (bar 2)) t nil))))

(define-test pprint-block.1
  (is equal
      "(BLOCK FU
  (WIBBLE 1)
  (QUUX 2))"
      (with-env (stream :right-margin 12)
        (inravina:pprint-block inravina:*client* stream
                               '(block fu (wibble 1) (quux 2)) t nil))))

(define-test pprint-block.2
  (is equal
      "(BLOCK
    FUBAR1
  (WIBBLE 1)
  (QUUX 2))"
      (with-env (stream :right-margin 12)
        (inravina:pprint-block inravina:*client* stream
                               '(block fubar1 (wibble 1) (quux 2)) t nil))))

(define-test pprint-function-call.1
  (is equal
      "(FU 1 2 3 :BAR 1 :WIBBLE 2)"
      (with-env (stream :right-margin 80)
        (inravina:pprint-function-call inravina:*client* stream
                                       '(fu 1 2 3 :bar 1 :wibble 2) t))))

(define-test pprint-function-call.2
  (is equal
      "(FU 1 2 3 :BAR
    1 :WIBBLE
    2)"
      (with-env (stream :right-margin 14)
        (inravina:pprint-function-call inravina:*client* stream
                                       '(fu 1 2 3 :bar 1 :wibble 2) t))))

(define-test pprint-function-call.3
  (is equal
      "(FU 1 2 3
    :BAR 1
    :WIBBLE 2)"
      (with-env (stream :right-margin 14)
        (inravina:pprint-function-call inravina:*client* stream
                                       '(fu 1 2 3 :bar 1 :wibble 2) t nil 3))))

(define-test pprint-let.1
  (is equal
      "(LET ((FU 1) (BAR 2)) (+ FU BAR))"
      (with-env (stream :right-margin 80)
        (inravina:pprint-let inravina:*client* stream
                             '(let ((fu 1) (bar 2)) (+ fu bar)) t))))

(define-test pprint-let.2
  (is equal
      "(LET ((FU 1)
      (BAR 2))
  (+ FU BAR))"
      (with-env (stream :right-margin 14)
        (inravina:pprint-let inravina:*client* stream
                             '(let ((fu 1) (bar 2)) (+ fu bar)) t))))

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

