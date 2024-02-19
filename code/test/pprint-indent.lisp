(in-package #:inravina/test)

(define-test pprint-indent.1
  (is equal
      "fu
 bar"
      (with-env (stream :right-margin 5)
        (inravina:pprint-logical-block (*client* stream nil)
          (inravina:pprint-indent *client* stream :block 1)
          (write-string "fu " stream)
          (inravina:pprint-newline *client* stream :fill)
          (inravina:pprint-indent *client* stream :block 2)
          (write-string "bar" stream)))))
