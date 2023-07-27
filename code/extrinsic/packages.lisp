(defpackage #:inravina-extrinsic
  (:use #:common-lisp)
  (:shadow #:*print-pprint-dispatch*
           #:copy-pprint-dispatch
           #:initialize
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
           #:set-pprint-dispatch)
  (:export #:*client*
           #:*print-pprint-dispatch*
           #:*standard-pprint-dispatch*
           #:copy-pprint-dispatch
           #:intrinsic-client
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
           #:pretty-stream-p
           #:set-pprint-dispatch))
