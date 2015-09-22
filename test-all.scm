(import (scheme base)
        (scheme eval)
        (scheme file)
        (srfi 1)
        (srfi 48)
        (srfi 64))

(test-begin "SRFI")

(for-each
 (lambda (n)
   (when (file-exists? (format #f "srfi-tests/~s.sld" n))
     (test-assert (string-append "SRFI-" (number->string n))
       (guard (err (else #f))
         (eval '(run-tests) (environment `(srfi-tests ,n)))))))
 (iota 200))

(test-end "SRFI")

(test-exit)
