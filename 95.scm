(define-library (srfi 95)
  (export sorted? merge merge! sort sort!)
  (import
   (except (scheme base) equal?)
   (srfi 63))
  (include "95.body.scm"))
