(defpackage #:inravina
  (:use #:common-lisp)
  (:documentation "A portable and extensible Common Lisp pretty printer.")
  (:shadow
    "COPY-PPRINT-DISPATCH"
    "PPRINT-DISPATCH"
    "PPRINT-EXIT-IF-LIST-EXHAUSTED"
    "PPRINT-FILL"
    "PPRINT-INDENT"
    "PPRINT-LINEAR"
    "PPRINT-LOGICAL-BLOCK"
    "PPRINT-NEWLINE"
    "PPRINT-POP"
    "PPRINT-TAB"
    "PPRINT-TABULAR"
    ;"*PRINT-PPRINT-DISPATCH*"
    "SET-PPRINT-DISPATCH")
  (:export
    #:client
    #:*client*
    #:copy-pprint-dispatch
    #:pprint-bindings
    #:pprint-block
    #:pprint-dispatch
    #:pprint-do
    #:pprint-eval-when
    #:pprint-exit-if-list-exhausted
    #:pprint-fill
    #:pprint-function-call
    #:pprint-indent
    #:pprint-let
    #:pprint-linear
    #:pprint-logical-block
    #:pprint-newline
    #:pprint-pop
    #:pprint-progn
    #:pprint-progv
    #:pprint-tab
    #:pprint-tabular
    #:pprint-tagbody
    ;#:*print-pprint-dispatch*
    #:set-pprint-dispatch))

