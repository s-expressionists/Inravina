(defpackage #:inravina-shim
  (:use #:common-lisp)
  (:export #:*client*
           #:*standard-pprint-dispatch*
           #:initialize-inravina
           #:shim-client
           #:pretty-stream-p))
