(define-library (srfi 37)
  (export
   args-fold
   option
   option-names
   option-required-arg?
   option-optional-arg?
   option-processor
   )
  (import
   (scheme base)
   (srfi 1))
  (include "37.body.scm"))
