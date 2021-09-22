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
    "*PRINT-PPRINT-DISPATCH*"
    "SET-PPRINT-DISPATCH")
  (:export
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
    #:*print-pprint-dispatch*
    #:set-pprint-dispatch))

