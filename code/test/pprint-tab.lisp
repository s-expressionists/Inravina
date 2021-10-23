(in-package #:inravina/test)

(define-test pprint-tab.1
  (is equal
      ":TEST :A"
      (with-output-to-string (stream)
        (inravina:pprint-logical-block (inravina:*client* stream '(:test :a))
          (inravina:pprint-exit-if-list-exhausted)
          (loop do (write (inravina:pprint-pop) :stream stream)
                   (inravina:pprint-exit-if-list-exhausted)
                   (inravina:pprint-tab inravina:*client* :line 1 1 stream))))))

