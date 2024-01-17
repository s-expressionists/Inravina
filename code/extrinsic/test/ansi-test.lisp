(in-package #:inravina-extrinsic/test)

(defvar *extrinsic-symbols*
  '(incless-extrinsic:pprint
    incless-extrinsic:prin1
    incless-extrinsic:prin1-to-string
    incless-extrinsic:princ
    incless-extrinsic:princ-to-string
    incless-extrinsic:print
    incless-extrinsic:print-object
    incless-extrinsic:print-unreadable-object
    incless-extrinsic:write
    incless-extrinsic:write-to-string
    inravina-extrinsic:*print-pprint-dispatch*
    inravina-extrinsic:copy-pprint-dispatch
    inravina-extrinsic:pprint-dispatch
    inravina-extrinsic:pprint-exit-if-list-exhausted
    inravina-extrinsic:pprint-fill
    inravina-extrinsic:pprint-indent
    inravina-extrinsic:pprint-linear
    inravina-extrinsic:pprint-logical-block
    inravina-extrinsic:pprint-newline
    inravina-extrinsic:pprint-pop
    inravina-extrinsic:pprint-tab
    inravina-extrinsic:pprint-tabular
    inravina-extrinsic:set-pprint-dispatch
    inravina-extrinsic:with-standard-io-syntax))

(defvar *tests*
  '("COPY-PPRINT-DISPATCH."
    "PPRINT"
    "SET-PPRINT-DISPATCH."))

(defun ansi-test (&rest args)
  (let ((system (asdf:find-system :inravina-extrinsic/test)))
    (apply #'ansi-test-harness:ansi-test
           :directory (merge-pathnames
                       (make-pathname :directory '(:relative
                                                   "dependencies"
                                                   "ansi-test"))
                       (asdf:component-pathname system))
           :expected-failures (asdf:component-pathname
                               (asdf:find-component system
                                                    '("code"
                                                      "expected-failures.sexp")))
           :extrinsic-symbols *extrinsic-symbols*
           :tests *tests*
           args)))
