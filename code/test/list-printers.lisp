(in-package #:inravina/test)

(defparameter +list-1+ '(:fu :bar :quux))
(defparameter +list-2+ '(:fu :bar . :quux))
(defparameter +plist-1+ '(:fu 1 :bar 2 :quux 3))

(define-test pprint-fill.1
  (is equal
      ":FU :BAR :QUUX"
      (with-env (stream :right-margin 80)
        (inravina:pprint-fill *client* stream +list-1+ nil nil)))
  (is equal
      ":FU :BAR
:QUUX"
      (with-env (stream :right-margin 10)
        (inravina:pprint-fill *client* stream +list-1+ nil nil)))
  (is equal
      "(:FU :BAR :QUUX)"
      (with-env (stream :right-margin 80)
        (inravina:pprint-fill *client* stream +list-1+ t nil)))
  (is equal
      "(:FU :BAR
 :QUUX)"
      (with-env (stream :right-margin 10)
        (inravina:pprint-fill *client* stream +list-1+ t nil))))

(define-test pprint-fill.2
  (is equal
      ":FU :BAR . :QUUX"
      (with-env (stream :right-margin 80)
        (inravina:pprint-fill *client* stream +list-2+ nil nil)))
  (is equal
      ":FU :BAR
. :QUUX"
      (with-env (stream :right-margin 10)
        (inravina:pprint-fill *client* stream +list-2+ nil nil)))
  (is equal
      "(:FU :BAR . :QUUX)"
      (with-env (stream :right-margin 80)
        (inravina:pprint-fill *client* stream +list-2+ t nil)))
  (is equal
      "(:FU :BAR
 . :QUUX)"
      (with-env (stream :right-margin 10)
        (inravina:pprint-fill *client* stream +list-2+ t nil))))

(define-test pprint-linear.1
  (is equal
      ":FU :BAR :QUUX"
      (with-env (stream :right-margin 80)
        (inravina:pprint-linear *client* stream +list-1+ nil nil)))
  (is equal
      ":FU
:BAR
:QUUX"
      (with-env (stream :right-margin 10)
        (inravina:pprint-linear *client* stream +list-1+ nil nil)))
  (is equal
      "(:FU :BAR :QUUX)"
      (with-env (stream :right-margin 80)
        (inravina:pprint-linear *client* stream +list-1+ t nil)))
  (is equal
      "(:FU
 :BAR
 :QUUX)"
      (with-env (stream :right-margin 10)
        (inravina:pprint-linear *client* stream +list-1+ t nil))))

(define-test pprint-linear.2
  (is equal
      ":FU :BAR . :QUUX"
      (with-env (stream :right-margin 80)
        (inravina:pprint-linear *client* stream +list-2+ nil nil)))
  (is equal
      ":FU
:BAR
. :QUUX"
      (with-env (stream :right-margin 10)
        (inravina:pprint-linear *client* stream +list-2+ nil nil)))
  (is equal
      "(:FU :BAR . :QUUX)"
      (with-env (stream :right-margin 80)
        (inravina:pprint-linear *client* stream +list-2+ t nil)))
  (is equal
      "(:FU
 :BAR
 . :QUUX)"
      (with-env (stream :right-margin 10)
        (inravina:pprint-linear *client* stream +list-2+ t nil))))

(define-test pprint-tabular.1
  (is equal
      ":FU   :BAR  :QUUX"
      (with-env (stream :right-margin 80)
        (inravina:pprint-tabular *client* stream +list-1+ nil nil 6)))
  (is equal
      ":FU   :BAR
:QUUX"
      (with-env (stream :right-margin 11)
        (inravina:pprint-tabular *client* stream +list-1+ nil nil 6)))
  (is equal
      "(:FU   :BAR  :QUUX)"
      (with-env (stream :right-margin 80)
        (inravina:pprint-tabular *client* stream +list-1+ t nil 6)))
  (is equal
    "(:FU   :BAR
 :QUUX)"
      (with-env (stream :right-margin 12)
        (inravina:pprint-tabular *client* stream +list-1+ t nil 6))))

(define-test pprint-tabular.2
  (is equal
      ":FU   :BAR  . :QUUX"
      (with-env (stream :right-margin 80)
        (inravina:pprint-tabular *client* stream +list-2+ nil nil 6)))
  (is equal
      ":FU   :BAR
. :QUUX"
      (with-env (stream :right-margin 11)
        (inravina:pprint-tabular *client* stream +list-2+ nil nil 6)))
  (is equal
      "(:FU   :BAR  . :QUUX)"
      (with-env (stream :right-margin 80)
        (inravina:pprint-tabular *client* stream +list-2+ t nil 6)))
  (is equal
    "(:FU   :BAR
 . :QUUX)"
      (with-env (stream :right-margin 12)
        (inravina:pprint-tabular *client* stream +list-2+ t nil 6))))

(define-test pprint-fill-plist
  (is equal
      ":FU 1 :BAR 2 :QUUX 3"
      (with-env (stream :right-margin 80)
        (inravina:pprint-fill-plist *client* stream +plist-1+ nil nil)))
  (is equal
      ":FU 1 :BAR 2
:QUUX 3"
      (with-env (stream :right-margin 16)
        (inravina:pprint-fill-plist *client* stream +plist-1+ nil nil)))
  (is equal
      "(:FU 1 :BAR 2 :QUUX 3)"
      (with-env (stream :right-margin 80)
        (inravina:pprint-fill-plist *client* stream +plist-1+ t nil)))
  (is equal
      "(:FU 1 :BAR 2
 :QUUX 3)"
      (with-env (stream :right-margin 17)
        (inravina:pprint-fill-plist *client* stream +plist-1+ t nil))))

(define-test pprint-linear-plist
  (is equal
      ":FU 1 :BAR 2 :QUUX 3"
      (with-env (stream :right-margin 80)
        (inravina:pprint-linear-plist *client* stream +plist-1+ nil nil)))
  (is equal
      ":FU 1
:BAR 2
:QUUX 3"
      (with-env (stream :right-margin 16)
        (inravina:pprint-linear-plist *client* stream +plist-1+ nil nil)))
  (is equal
      "(:FU 1 :BAR 2 :QUUX 3)"
      (with-env (stream :right-margin 80)
        (inravina:pprint-linear-plist *client* stream +plist-1+ t nil)))
  (is equal
      "(:FU 1
 :BAR 2
 :QUUX 3)"
      (with-env (stream :right-margin 17)
        (inravina:pprint-linear-plist *client* stream +plist-1+ t nil))))

(define-test pprint-tabular-plist
  (is equal
      ":FU 1   :BAR    2   :QUUX   3"
      (with-env (stream :right-margin 80)
        (inravina:pprint-tabular-plist *client* stream +plist-1+ nil nil 4)))
  (is equal
      ":FU 1   :BAR    2
:QUUX   3"
      (with-env (stream :right-margin 24)
        (inravina:pprint-tabular-plist *client* stream +plist-1+ nil nil 4)))
  (is equal
      "(:FU 1   :BAR    2   :QUUX   3)"
      (with-env (stream :right-margin 80)
        (inravina:pprint-tabular-plist *client* stream +plist-1+ t nil 4)))
  (is equal
      "(:FU 1   :BAR    2
 :QUUX   3)"
      (with-env (stream :right-margin 25)
        (inravina:pprint-tabular-plist *client* stream +plist-1+ t nil 4))))

