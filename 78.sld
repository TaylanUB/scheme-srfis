(define-library (srfi 78)
  (export
   check
   check-ec
   check-report
   check-set-mode!
   check-reset!
   check-passed?
   )
  (import
   (scheme base)
   (scheme cxr)
   (scheme write)
   (srfi 42))
  (include "78.upstream.scm"))
