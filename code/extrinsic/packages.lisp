(defpackage #:inravina-extrinsic
  (:use #:common-lisp)
  (:shadow #:copy-pprint-dispatch
           #:pprint-dispatch
           #:pprint-exit-if-list-exhausted
           #:pprint-fill
           #:pprint-indent
           #:pprint-linear
           #:pprint-logical-block
           #:pprint-newline
           #:pprint-pop
           #:pprint-tab
           #:pprint-tabular
           #:*print-pprint-dispatch*
           #:set-pprint-dispatch)
  (:export #:copy-pprint-dispatch
           #:pprint-dispatch
           #:pprint-exit-if-list-exhausted
           #:pprint-fill
           #:pprint-indent
           #:pprint-linear
           #:pprint-logical-block
           #:pprint-newline
           #:pprint-pop
           #:pprint-tab
           #:pprint-tabular
           #:*print-pprint-dispatch*
           #:set-pprint-dispatch))

