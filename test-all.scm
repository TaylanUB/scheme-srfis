(import (scheme base)
        (scheme eval)
        (scheme file)
        (scheme process-context)
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

(let ((runner (test-runner-current)))
  (unless (and (= 0 (test-runner-xpass-count runner))
               (= 0 (test-runner-fail-count runner)))
    (exit 1)))
