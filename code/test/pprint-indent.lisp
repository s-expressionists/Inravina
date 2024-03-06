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
        (inravina:pprint-logical-block (*client* stream nil :per-line-prefix "Z")
          (write-string "A " stream)
          (inravina:pprint-newline *client* stream :mandatory)
          (write-string "B
C" stream)
          (inravina:pprint-logical-block (*client* stream nil :prefix "[" :suffix "]")
            (write-string "D " stream)
            (inravina:pprint-newline *client* stream :mandatory)
            (write-string "E
F" stream))))))

(define-test pprint-logical-block.1
  (is equal
      "a .."
      (with-env (stream :lines 1)
        (inravina:pprint-logical-block (*client* stream nil :prefix "a
(" :suffix ")")
          (write-string "wibble" stream)
          (inravina:pprint-newline *client* stream :mandatory)
          (write-string "bar" stream)))))

(define-test pprint-logical-block.2
  (is equal
      "a
(wibble ..)"
      (with-env (stream :lines 2)
        (inravina:pprint-logical-block (*client* stream nil :prefix "a
(" :suffix ")")
          (write-string "wibble" stream)
          (inravina:pprint-newline *client* stream :mandatory)
          (write-string "bar" stream)))))

(define-test pprint-logical-block.3
  (is equal
      "za .."
      (with-env (stream :lines 1)
        (inravina:pprint-logical-block (*client* stream nil :per-line-prefix "z")
          (inravina:pprint-logical-block (*client* stream nil :prefix "a
(" :suffix ")")
            (write-string "wibble" stream)
            (inravina:pprint-newline *client* stream :mandatory)
            (write-string "bar" stream))))))

(define-test pprint-logical-block.4
  (is equal
      "za ..
z(wibble ..)"
      (with-env (stream :lines 2)
        (inravina:pprint-logical-block (*client* stream nil :per-line-prefix "z")
          (inravina:pprint-logical-block (*client* stream nil :prefix "a
(" :suffix ")")
            (write-string "wibble" stream)
            (inravina:pprint-newline *client* stream :mandatory)
            (write-string "bar" stream))))))

(define-test pprint-logical-block.5
  (is equal
      "[a
(wibble
 bar)]"
      (with-env (stream :lines 2)
        (inravina:pprint-logical-block (*client* stream nil :prefix "[" :suffix "]")
          (inravina:pprint-logical-block (*client* stream nil :prefix "a
(" :suffix ")")
            (write-string "wibble" stream)
            (inravina:pprint-newline *client* stream :mandatory)
            (write-string "bar" stream))))))
