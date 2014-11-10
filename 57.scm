(define-library (srfi 57)
  (export
   define-record-type
   define-record-scheme
   record-update
   record-update!
   record-compose
   )
  (import
   (rename (scheme base) (define-record-type srfi-9:define-record-type))
   (scheme case-lambda))
  (include "57.upstream.scm"))
