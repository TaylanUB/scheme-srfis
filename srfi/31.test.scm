(import (scheme base)
        (scheme lazy)
        (srfi 31)
        (srfi 64))

(test-begin "srfi 31")

(test-eqv 3628800
          ((rec (! n)
                (if (zero? n)
                    1
                    (* n (! (- n 1)))))
           10))

(test-eqv 'x
          (car (force (cdr (force (cdr (rec xs (cons 'x (delay xs)))))))))

(test-end "srfi 31")
