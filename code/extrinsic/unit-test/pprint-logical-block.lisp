(in-package #:inravina-extrinsic/unit-test)

(define-test pprint-logical-block.1
  (is equal
      "ZA
ZB
ZC[D
Z  E
ZF]"
      (with-env (stream)
        (inravina-extrinsic:pprint-logical-block (stream nil :per-line-prefix "Z")
          (write-string "A " stream)
          (inravina-extrinsic:pprint-newline :mandatory stream)
          (write-string "B
C" stream)
          (inravina-extrinsic:pprint-logical-block (stream nil :prefix "[" :suffix "]")
            (write-string "D " stream)
            (inravina-extrinsic:pprint-newline :mandatory stream)
            (write-string "E
F" stream))))))


(define-test pprint-logical-block.1
  (is equal
      "a .."
      (with-env (stream :lines 1)
        (inravina-extrinsic:pprint-logical-block (stream nil :prefix "a
(" :suffix ")")
          (write-string "wibble" stream)
          (inravina-extrinsic:pprint-newline :mandatory stream)
          (write-string "bar" stream)))))

(define-test pprint-logical-block.2
  (is equal
      "a
(wibble ..)"
      (with-env (stream :lines 2)
        (inravina-extrinsic:pprint-logical-block (stream nil :prefix "a
(" :suffix ")")
          (write-string "wibble" stream)
          (inravina-extrinsic:pprint-newline :mandatory stream)
          (write-string "bar" stream)))))

(define-test pprint-logical-block.3
  (is equal
      "za .."
      (with-env (stream :lines 1)
        (inravina-extrinsic:pprint-logical-block (stream nil :per-line-prefix "z")
          (inravina-extrinsic:pprint-logical-block (stream nil :prefix "a
(" :suffix ")")
            (write-string "wibble" stream)
            (inravina-extrinsic:pprint-newline :mandatory stream)
            (write-string "bar" stream))))))

(define-test pprint-logical-block.4
  (is equal
      "za
z(wibble ..)"
      (with-env (stream :lines 2)
        (inravina-extrinsic:pprint-logical-block (stream nil :per-line-prefix "z")
          (inravina-extrinsic:pprint-logical-block (stream nil :prefix "a
(" :suffix ")")
            (write-string "wibble" stream)
            (inravina-extrinsic:pprint-newline :mandatory stream)
            (write-string "bar" stream))))))

(define-test pprint-logical-block.5
  (is equal
      "[a
(wibble ..)]"
      (with-env (stream :lines 2)
        (inravina-extrinsic:pprint-logical-block (stream nil :prefix "[" :suffix "]")
          (inravina-extrinsic:pprint-logical-block (stream nil :prefix "a
(" :suffix ")")
            (write-string "wibble" stream)
            (inravina-extrinsic:pprint-newline :mandatory stream)
            (write-string "bar" stream))))))
