(in-package #:inravina/test)

(define-test pprint-bindings.1
  (is equal
      "((FU 1) (BAR 2))"
      (with-env (stream :right-margin 80)
        (inravina:pprint-bindings *client* stream
                                  '((fu 1) (bar 2))))))
(define-test pprint-bindings.2
  (is equal
      "((FU 1)
 (BAR 2))"
      (with-env (stream :right-margin 9)
        (inravina:pprint-bindings *client* stream
                                  '((fu 1) (bar 2))))))
(define-test pprint-bindings.3
  (is equal
      "((FU 1)
 (BAR
  2))"
      (with-env (stream :right-margin 8)
        (inravina:pprint-bindings *client* stream
                                  '((fu 1) (bar 2))))))
(define-test pprint-bindings.4
  (is equal
      "((FU
  1)
 (BAR
  2))"
      (with-env (stream :right-margin 6)
        (inravina:pprint-bindings *client* stream
                                  '((fu 1) (bar 2))))))

(define-test pprint-block.1
  (is equal
      "(BLOCK FU
  (WIBBLE 1)
  (QUUX 2))"
      (with-env (stream :right-margin 12)
        (inravina::pprint-prog1 *client* stream
                               '(block fu (wibble 1) (quux 2))))))

(define-test pprint-block.2
  (is equal
      "(BLOCK FUBAR1
  (WIBBLE 1)
  (QUUX 2))"
      (with-env (stream :right-margin 12)
        (inravina::pprint-prog1 *client* stream
                               '(block fubar1 (wibble 1) (quux 2))))))

(define-test pprint-do.1
  (is equal
      "(DO ((FU 1 (INCF FU)) (BAR 2)) ((= FU 10) BAR) QUUX (INCF BAR FU))"
      (with-env (stream :right-margin 80)
        (inravina:pprint-do *client* stream
                            '(do ((fu 1 (incf fu)) (bar 2)) ((= fu 10) bar) quux (incf bar fu))))))

(define-test pprint-do.2
  (is equal
      "(DO ((FU 1 (INCF FU))
     (BAR 2))
    ((= FU 10) BAR)
 QUUX
  (INCF BAR FU))"
      (with-env (stream :right-margin 22)
        (inravina:pprint-do *client* stream
                            '(do ((fu 1 (incf fu)) (bar 2)) ((= fu 10) bar) quux (incf bar fu))))))

(define-test pprint-do.3
  (is equal
      "(DO* ((FU 1 (INCF FU))
      (BAR 2))
     ((= FU 10) BAR)
 QUUX
  (INCF BAR FU))"
      (with-env (stream :right-margin 23)
        (inravina:pprint-do *client* stream
                            '(do* ((fu 1 (incf fu)) (bar 2)) ((= fu 10) bar) quux (incf bar fu))))))

(define-test pprint-function-call.1
  (is equal
      "(FU 1 2 3 :BAR 1 :WIBBLE 2)"
      (with-env (stream :right-margin 80)
        (inravina:pprint-function-call *client* stream
                                       '(fu 1 2 3 :bar 1 :wibble 2)))))

(define-test pprint-function-call.2
  (is equal
      "(FU 1 2 3 :BAR
    1 :WIBBLE
    2)"
      (with-env (stream :right-margin 15)
        (inravina:pprint-function-call *client* stream
                                       '(fu 1 2 3 :bar 1 :wibble 2)))))

(define-test pprint-function-call.3
  (is equal
      "(FU 1 2 3
    :BAR 1
    :WIBBLE 2)"
      (with-env (stream :right-margin 14)
        (inravina:pprint-function-call *client* stream
                                       '(fu 1 2 3 :bar 1 :wibble 2) :argument-count 3))))

(define-test pprint-eval-when
  (let ((form '(eval-when (:fu :bar :quux) (wibble 1))))
    (is equal
        "(EVAL-WHEN (:FU :BAR :QUUX) (WIBBLE 1))"
        (with-env (stream :right-margin 80)
          (inravina:pprint-eval-when *client* stream form)))
    (is equal
        "(EVAL-WHEN (:FU
            :BAR
            :QUUX)
  (WIBBLE 1))"
        (with-env (stream :right-margin 20)
          (inravina:pprint-eval-when *client* stream form)))))

(define-test pprint-let.1
  (is equal
      "(LET ((FU 1) (BAR 2)) (+ FU BAR))"
      (with-env (stream :right-margin 80)
        (inravina:pprint-let *client* stream
                             '(let ((fu 1) (bar 2)) (+ fu bar))))))

(define-test pprint-let.2
  (is equal
      "(LET ((FU 1)
      (BAR 2))
  (+ FU BAR))"
      (with-env (stream :right-margin 15)
        (inravina:pprint-let *client* stream
                             '(let ((fu 1) (bar 2)) (+ fu bar))))))

(define-test pprint-let.3
  (is equal
      "(LET* ((FU 1)
       (BAR 2))
  (+ FU BAR))"
      (with-env (stream :right-margin 16)
        (inravina:pprint-let *client* stream
                             '(let* ((fu 1) (bar 2)) (+ fu bar))))))

(define-test pprint-progn
  (let ((form '(progn (+ fu bar) bar)))
    (is equal
        "(PROGN (+ FU BAR) BAR)"
        (with-env (stream :right-margin 80)
          (inravina:pprint-progn *client* stream form)))
    (is equal
        "(PROGN
  (+ FU BAR)
  BAR)"
        (with-env (stream :right-margin 12)
          (inravina:pprint-progn *client* stream form)))))

(define-test pprint-tagbody.1
  (is equal
      "(TAGBODY
 FU
  (WIBBLE 1)
 BAR
  (QUUX 2))"
      (with-env (stream :right-margin 12)
        (inravina:pprint-tagbody *client* stream
                                 '(tagbody fu (wibble 1) bar (quux 2))))))

(define-test pprint-argument-list
  (let ((form '(1 2 3 :fu 1 :bar 2 :quux 3)))
    (is equal
        "(1 2 3 :FU 1 :BAR 2 :QUUX 3)"
        (with-env (stream :right-margin 80)
          (inravina:pprint-argument-list *client* stream form)))
    (is equal
        "(1 2 3 :FU
 1 :BAR 2
 :QUUX 3)"
        (with-env (stream :right-margin 11)
          (inravina:pprint-argument-list *client* stream form)))
    (is equal
        "(1 2 3
 :FU 1
 :BAR 2
 :QUUX 3)"
        (with-env (stream :right-margin 10)
          (inravina:pprint-argument-list *client* stream form :argument-count 3)))
    (is equal
        "(1 2 3 :FU 1
 :BAR 2 :QUUX 3)"
        (with-env (stream :right-margin 16)
          (inravina:pprint-argument-list *client* stream form :argument-count 3)))))

(define-test pprint-with
  (let ((form '(fu (bar quux wibble) quux gronk)))
    (is equal
        "(FU (BAR QUUX WIBBLE) QUUX GRONK)"
        (with-env (stream :right-margin 80)
          (inravina:pprint-with *client* stream form)))
    (is equal
        "(FU (BAR QUUX
     WIBBLE)
  QUUX
  GRONK)"
        (with-env (stream :right-margin 14)
          (inravina:pprint-with *client* stream form)))))

(define-test pprint-lambda-list
  (let ((form '(x y z &key fu (bar nil bar-p) &aux a (b 4))))
    (is equal
        "(X Y Z &KEY FU (BAR NIL BAR-P) &AUX A (B 4))"
        (with-env (stream :right-margin 80)
          (inravina:pprint-lambda-list *client* stream form)))
    (is equal
        "(X Y Z
 &KEY FU (BAR NIL BAR-P)
 &AUX A (B 4))"
        (with-env (stream :right-margin 40)
          (inravina:pprint-lambda-list *client* stream form)))
    (is equal
        "(X Y Z
 &KEY FU
      (BAR
       NIL
       BAR-P)
 &AUX A (B 4))"
        (with-env (stream :right-margin 20)
          (inravina:pprint-lambda-list *client* stream form)))
    (is equal
        "(X Y Z
 &KEY FU
      (BAR
       NIL
       BAR-P)
 &AUX A
      (B 4))"
        (with-env (stream :right-margin 12)
          (inravina:pprint-lambda-list *client* stream form)))))

(define-test pprint-extended-loop
  (let ((form '(loop named fu
                     with bar
                          and baz = 7
                          and quux integer = 9
                     initially (wibble 1)
                               (gronk t)
                     finally (wibble baz)
                             (gronk bar)
                     for i from 1
                         and j below 20
                     for k in z
                     doing (zap k)
                           (wibble z)
                     collect z into q
                     when (zerop bar)
                       collect bar
                       and do (zum bar)
                              (zim bar)
                     else when (never k)
                       maximize k
                     else
                       minimize k
                     end
                     when (zero baz)
                       do (something baz))))

    (is equal
"(LOOP NAMED FU
      WITH BAR
           AND BAZ = 7
           AND QUUX INTEGER = 9
      INITIALLY (WIBBLE 1)
                (GRONK T)
      FINALLY (WIBBLE BAZ)
              (GRONK BAR)
      FOR I FROM 1
          AND J BELOW 20
      FOR K IN Z
      DOING (ZAP K)
            (WIBBLE Z)
      COLLECT Z INTO Q
      WHEN (ZEROP BAR)
        COLLECT BAR
        AND DO (ZUM BAR)
               (ZIM BAR)
      ELSE WHEN (NEVER K)
        MAXIMIZE K
      ELSE
        MINIMIZE K
      END
      WHEN (ZERO BAZ)
        DO (SOMETHING BAZ))"
        (with-env (stream :right-margin 80)
          (inravina:pprint-extended-loop *client* stream form)))))

