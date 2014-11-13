(define-library (srfi 41 primitive)
  (export
   stream-null stream-cons stream? stream-null? stream-pair?
   stream-car stream-cdr stream-lambda
   )
  (import (scheme base))
  (include "primitive.body.scm"))
