(in-package #:inravina/test)

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

(define-test pprint-do.1
  (is equal
      "(DO ((FU 1 (INCF FU)) (BAR 2)) ((= FU 10) BAR) QUUX (INCF BAR FU))"
      (with-env (stream :right-margin 80)
        (inravina:pprint-do inravina:*client* stream
                            '(do ((fu 1 (incf fu)) (bar 2)) ((= fu 10) bar) quux (incf bar fu)) t))))

(define-test pprint-do.2
  (is equal
      "(DO ((FU 1 (INCF FU))
     (BAR 2))
    ((= FU 10) BAR)
 QUUX
  (INCF BAR FU))"
      (with-env (stream :right-margin 21)
        (inravina:pprint-do inravina:*client* stream
                            '(do ((fu 1 (incf fu)) (bar 2)) ((= fu 10) bar) quux (incf bar fu)) t))))

(define-test pprint-do.3
  (is equal
      "(DO* ((FU 1 (INCF FU))
      (BAR 2))
     ((= FU 10) BAR)
 QUUX
  (INCF BAR FU))"
      (with-env (stream :right-margin 22)
        (inravina:pprint-do inravina:*client* stream
                            '(do* ((fu 1 (incf fu)) (bar 2)) ((= fu 10) bar) quux (incf bar fu)) t))))

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

(define-test pprint-eval-when
  (let ((form '(eval-when (:fu :bar :quux) (wibble 1))))
    (is equal
        "(EVAL-WHEN (:FU :BAR :QUUX) (WIBBLE 1))"
        (with-env (stream :right-margin 80)
          (inravina:pprint-eval-when inravina:*client* stream form t)))
    (is equal
        "(EVAL-WHEN (:FU :BAR
            :QUUX)
  (WIBBLE 1))"
        (with-env (stream :right-margin 20)
          (inravina:pprint-eval-when inravina:*client* stream form t)))))

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

(define-test pprint-let.3
  (is equal
      "(LET* ((FU 1)
       (BAR 2))
  (+ FU BAR))"
      (with-env (stream :right-margin 15)
        (inravina:pprint-let inravina:*client* stream
                             '(let* ((fu 1) (bar 2)) (+ fu bar)) t))))

(define-test pprint-progn
  (let ((form '(progn (+ fu bar) bar)))
    (is equal
        "(PROGN (+ FU BAR) BAR)"
        (with-env (stream :right-margin 80)
          (inravina:pprint-progn inravina:*client* stream form t)))
    (is equal
        "(PROGN
  (+ FU BAR)
  BAR)"
        (with-env (stream :right-margin 11)
          (inravina:pprint-progn inravina:*client* stream form t)))))

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

(define-test pprint-argument-list
  (let ((form '(1 2 3 :fu 1 :bar 2 :quux 3)))
    (is equal
        "(1 2 3 :FU 1 :BAR 2 :QUUX 3)"
        (with-env (stream :right-margin 80)
          (inravina:pprint-argument-list inravina:*client* stream form t nil nil)))
    (is equal
        "(1 2 3 :FU
 1 :BAR 2
 :QUUX 3)"
        (with-env (stream :right-margin 10)
          (inravina:pprint-argument-list inravina:*client* stream form t nil nil)))
    (is equal
        "(1 2 3
 :FU 1
 :BAR 2
 :QUUX 3)"
        (with-env (stream :right-margin 10)
          (inravina:pprint-argument-list inravina:*client* stream form t nil 3)))
    (is equal
        "(1 2 3 :FU 1
 :BAR 2 :QUUX 3)"
        (with-env (stream :right-margin 16)
          (inravina:pprint-argument-list inravina:*client* stream form t nil 3)))))

(define-test pprint-with-hash-table-iterator
  (let ((form '(fu (bar quux wibble) quux gronk)))
    (is equal
        "(FU (BAR QUUX WIBBLE) QUUX GRONK)"
        (with-env (stream :right-margin 80)
          (inravina:pprint-with-hash-table-iterator inravina:*client* stream form t nil)))
    (is equal
        "(FU (BAR QUUX
     WIBBLE)
  QUUX
  GRONK)"
        (with-env (stream :right-margin 13)
          (inravina:pprint-with-hash-table-iterator inravina:*client* stream form t nil)))))

(define-test pprint-lambda-list
  (let ((form '(x y z &key fu (bar nil bar-p) &aux a (b 4))))
    (is equal
        "(X Y Z &KEY FU (BAR NIL BAR-P) &AUX A (B 4))"
        (with-env (stream :right-margin 80)
          (inravina:pprint-lambda-list inravina:*client* stream form t nil)))
    (is equal
        "(X Y Z
 &KEY FU (BAR NIL BAR-P)
 &AUX A (B 4))"
        (with-env (stream :right-margin 40)
          (inravina:pprint-lambda-list inravina:*client* stream form t nil)))
    (is equal
        "(X Y Z
 &KEY FU (BAR
          NIL
          BAR-P)
 &AUX A (B 4))"
        (with-env (stream :right-margin 20)
          (inravina:pprint-lambda-list inravina:*client* stream form t nil)))
    (is equal
        "(X Y Z
 &KEY FU
      (BAR
       NIL
       BAR-P)
 &AUX A (B
         4))"
        (with-env (stream :right-margin 12)
          (inravina:pprint-lambda-list inravina:*client* stream form t nil)))))

