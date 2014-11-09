(define-library (srfi 51)
  (export
   rest-values
   arg-and
   arg-ands
   err-and
   err-ands
   arg-or
   arg-ors
   err-or
   err-ors
   )
  (import
   (scheme base)
   (srfi 1))
  (include "51.upstream.scm"))
