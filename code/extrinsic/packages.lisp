(defpackage #:inravina-extrinsic
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
           #:intrinsic-client
           #:initialize-inravina
           #:pprint-apply
           #:pprint-argument-list
           #:pprint-array
           #:pprint-bindings
           #:pprint-call
           #:pprint-case
           #:pprint-cond
           #:pprint-defclass
           #:pprint-defmethod
           #:pprint-defpackage
           #:pprint-defun
           #:pprint-destructuring-bind
           #:pprint-dispatch
           #:pprint-do
           #:pprint-dolist
           #:pprint-exit-if-list-exhausted
           #:pprint-fill
           #:pprint-flet
           #:pprint-function-call
           #:pprint-indent
           #:pprint-lambda
           #:pprint-lambda-list
           #:pprint-let
           #:pprint-linear
           #:pprint-logical-block
           #:pprint-loop
           #:pprint-macro-char
           #:pprint-multiple-value-bind
           #:pprint-newline
           #:pprint-pop
           #:pprint-prog
           #:pprint-prog1
           #:pprint-prog2
           #:pprint-progn
           #:pprint-progv
           #:pprint-symbol-macrolet
           #:pprint-tab
           #:pprint-tabular
           #:pprint-tagbody
           #:pprint-with
           #:pretty-stream-p
           #:set-pprint-dispatch
           #:with-standard-io-syntax))
