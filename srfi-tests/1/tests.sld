(define-library (srfi-tests 1 tests)
  (export run-tests)
  (import (scheme base)
          (srfi 1))
  (begin
    (define (run-tests)
      (and
       (equal? (list 3 2 1) (fold cons '() (list 1 2 3)))
       (= (find even? '(3 1 4 1 5 9)) 4)))))
