(define-library (srfi 71)
  (export
   (rename srfi-letrec* letrec*)
   (rename srfi-letrec letrec)
   (rename srfi-let* let*)
   (rename srfi-let let)
   uncons
   uncons-2
   uncons-3
   uncons-4
   uncons-cons
   unlist
   unvector
   )
  (import
   (rename (scheme base)
           (let r5rs-let)
           (letrec r5rs-letrec))
   (scheme cxr))
  (include "71.upstream.scm"))
