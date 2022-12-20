(in-package #:inravina/test)

(define-test pprint-tab.1
  (is equal
      ":TEST :A"
      (with-output-to-string (stream)
        (inravina:pprint-logical-block (inravina:*client* stream '(:test :a))
          (inravina:pprint-exit-if-list-exhausted)
          (loop do (incless/core:write-object inravina:*client* (inravina:pprint-pop) stream)
                   (inravina:pprint-exit-if-list-exhausted)
                   (inravina:pprint-tab inravina:*client* stream :line 1 1))))))

