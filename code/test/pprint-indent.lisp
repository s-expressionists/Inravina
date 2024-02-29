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

(define-test pprint-newline.1
  (is equal
      "ZA
ZB
ZC[D
Z  E
ZF]"
      (with-env (stream)
        (inravina:pprint-logical-block (*client* stream  nil :per-line-prefix "Z")
          (write-string "A " stream)
          (inravina:pprint-newline *client* stream :mandatory)
          (write-string "B
C" stream)
          (inravina:pprint-logical-block (*client* stream nil :prefix "[" :suffix "]")
            (write-string "D " stream)
            (inravina:pprint-newline *client* stream :mandatory)
            (write-string "E
F" stream))))))
