(import (scheme base)
        (scheme eval)
        (scheme file)
        (srfi 1)
        (srfi 48)
        (srfi 64))

(test-runner-current (test-runner-simple "tests.log"))

(test-begin "SRFI")

(for-each
 (lambda (n)
   (let ((srfi-n (string->symbol (format #f "srfi-~s" n)))
         (file-name (format #f "srfi-tests/srfi-~s.sld" n))
         (test-name (format #f "SRFI-~s" n)))
    (when (file-exists? file-name)
      (test-assert test-name
        (guard (err (else #f))
          (eval '(run-tests) (environment `(srfi-tests ,srfi-n))))))))
 (iota 200))

(test-end "SRFI")

(test-exit)
