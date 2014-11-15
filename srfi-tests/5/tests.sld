(define-library (srfi-tests 5 tests)
  (export run-tests)
  (import (except (scheme base) let)
          (scheme write)
          (srfi 5))
  (begin
    (define (run-tests)
      (let (blast (port (open-output-string)) . (x (+ 1 2) 4 5))
        (if (null? x)
            (equal? (get-output-string port) "345")
            (begin
              (write (car x) port)
              (apply blast port (cdr x))))))))
