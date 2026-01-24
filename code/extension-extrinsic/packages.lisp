(defpackage #:inravina-extension-extrinsic
  (:use #:common-lisp)
  (:shadow #:*print-pprint-dispatch*
           #:copy-pprint-dispatch
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
           #:set-pprint-dispatch
           #:with-standard-io-syntax)
  (:export #:*client*
           #:*print-pprint-dispatch*
           #:*standard-pprint-dispatch*
           #:extrinsic-client
           #:copy-pprint-dispatch
           #:initialize-inravina
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
           #:set-pprint-dispatch
           #:with-standard-io-syntax))
