(define-library (srfi 25)
  (export
   array?
   make-array
   shape
   array
   array-rank
   array-start
   array-end
   array-ref
   array-set!
   share-array
   )
  (import
   (scheme base)
   (scheme write))
  (include "25.as-srfi-9-record.upstream.scm")
  (include "25.ix-ctor.upstream.scm")
  (include "25.op-ctor.upstream.scm")
  (include "25.main.upstream.scm"))
